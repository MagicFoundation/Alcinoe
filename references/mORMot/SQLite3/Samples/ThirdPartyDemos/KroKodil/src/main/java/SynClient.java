import org.json.JSONArray;
import org.json.JSONObject;

import java.security.MessageDigest;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

class SynException extends Exception {
    public SynException(int errorCode, String errorText) {
        super(String.format("Server side error: %d %s", errorCode, errorText));
    }
}

public class SynClient {

    private String user;
    private String root;
    private RESTClient http;

    private long userID;
    private String logonDisplay;
    private int timeout;
    private String version;

    private long sessionID = 0;
    private String passwordHashHexa = "";
    private String sessionIDHexa8 =  "";
    private long sessionPrivateKey  = 0;
    private long sessionTickCountOffset = 0;
    private long serverTimeStampOffset;
    private long lastSessionTickCount = 0;

    public static int HTTP_OK = 200;
    public static int HTTP_FORBIDDEN = 403;

    public SynClient(String host, int port, String root, boolean ssl){
        this.root = root;
        String baseUrl = (ssl?"https":"http") + "://" + host + ":" + port + "/";
        this.http =  new RESTClient(baseUrl);
    }

    public long getTimestamp() {
        return this.nowAsMormotTime() + this.serverTimeStampOffset;
    }
    public boolean login(String user, String password) throws Exception {
        this.user = user;
        this.passwordHashHexa = SHA256("salt" + password);
        this.sessionTickCountOffset = System.currentTimeMillis();
        return doAuth();
    }

    public JSONObject call(String function, JSONArray data) throws Exception {
        JSONObject response = http.doPost(signUrl(root + "/" + function), data);
        if (http.statusCode == HTTP_FORBIDDEN) {
            if (doAuth()) {
                response = http.doPost(signUrl(root +  "/" + function), data);
            }
        }

        if (http.statusCode != HTTP_OK)
            throw new SynException(response.getInt("errorCode"),
                                   response.getString("errorText") );

        return response;
    }

    public void logout() throws Exception {
        String url = signUrl(root + "/auth?UserName="+ this.user + "&Session=" + this.sessionID);
        String response = http.doGet(url);
        if (http.statusCode == HTTP_OK) {
            sessionID = 0;
            passwordHashHexa = "";
            sessionIDHexa8 =  "";
            sessionPrivateKey  = 0;
        } else {
            JSONObject json = new JSONObject(response);
            throw new SynException(json.getInt("errorCode"),
                    json.getString("errorText"));
        }

    }

    private  String byteToHex(byte[] bts) {
        StringBuilder des = new StringBuilder();
        String tmp;
        for (byte bt : bts) {
            tmp = (Integer.toHexString(bt & 0xFF));
            if (tmp.length() == 1) {
                des.append("0");
            }
            des.append(tmp);
        }
        return des.toString();
    }

    private String SHA256(String nonce) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] bytes = nonce.getBytes();
            md.update(bytes);
            return byteToHex(md.digest());
        }catch (java.security.NoSuchAlgorithmException err) {
            err.printStackTrace();
        }
        return "";
    }

    private String padL(String s, int count) {
        String result = String.format("%"+count+"s", s).replace(' ','0');
        if (result.length() > count)
            return  result.substring(result.length() - count);
        else
            return result;
    }

    private String binaryString(int i, int digits) {
        return padL(Integer.toBinaryString(i), digits);
    }

    private long nowAsMormotTime() {
        Calendar cal = Calendar.getInstance();
        String clientTime = binaryString( cal.get(Calendar.YEAR), 13);
        clientTime += binaryString( cal.get(Calendar.MONTH), 4);
        clientTime += binaryString( cal.get(Calendar.DAY_OF_MONTH)-1, 5);

        clientTime += binaryString( cal.get(Calendar.HOUR_OF_DAY), 5);
        clientTime += binaryString( cal.get(Calendar.MINUTE), 6);
        clientTime += binaryString( cal.get(Calendar.SECOND), 6);

        return Long.valueOf(clientTime, 2);
    }

    private void gotTimestamp(String timestamp) {
        serverTimeStampOffset = Long.parseLong(timestamp) - nowAsMormotTime();
    }

    private String calcClientNonce () {
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String clientNonce = df.format(new Date());
        return SHA256(clientNonce);
    }

    private long  gotSession(String sessionKey) {
        int i = sessionKey.indexOf('+');
        sessionID = Long.valueOf(sessionKey.substring(0, i));
        sessionIDHexa8 = padL(Long.toHexString(sessionID), 8);

        long r = CRC32.calculate(sessionKey, 0);
        sessionPrivateKey = CRC32.calculate(passwordHashHexa, r);

        return sessionPrivateKey;
    }

    private boolean doAuth() throws Exception {
        String timeStamp =  http.doGet(root + "/TimeStamp");

        gotTimestamp(timeStamp);

        String response = http.doGet(root + "/auth?UserName="+this.user);
        JSONObject json = new JSONObject(response);
        if (http.statusCode != HTTP_OK)
            throw new SynException(json.getInt("errorCode"), json.getString("errorText") );

        String serverNonce = json.getString("result");
        String clientNonce =  calcClientNonce();
        response =  http.doGet(root +"/auth"+
                 "?UserName="+ user +
                 "&Password="+ SHA256(root + serverNonce + clientNonce + user + passwordHashHexa)+
                 "&ClientNonce=" + clientNonce);

        json = new JSONObject(response);
        if (http.statusCode != HTTP_OK)
           throw new SynException(json.getInt("errorCode"), json.getString("errorText") );

        userID = json.getInt("logonid");
        logonDisplay = json.getString("logondisplay");
        version = json.getString("version");
        timeout = json.getInt("timeout");
        if (timeout == 0) timeout = 60;

        return gotSession(json.getString("result")) > 0;
    }

    private String signUrl(String url) {
        long ticks = System.currentTimeMillis() >> 8; // 256 ms resolution;

        if (lastSessionTickCount == ticks)
            lastSessionTickCount += 1;
        else
            lastSessionTickCount = ticks;

        String nonce = padL(Long.toHexString(lastSessionTickCount), 8);

        long signature = CRC32.calculate(url, CRC32.calculate(nonce, sessionPrivateKey));
        String sessionSignature = sessionIDHexa8 + nonce +padL(Long.toHexString(signature), 8);

        return url + (url.contains("?") ? "&session_signature=":
                                          "?session_signature=") + sessionSignature;
    }

}