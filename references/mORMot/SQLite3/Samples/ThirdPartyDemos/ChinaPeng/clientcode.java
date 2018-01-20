package com.core;

import android.util.Log;
import android.util.Xml;
import org.apache.http.util.EncodingUtils;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.LineNumberReader;
import java.security.MessageDigest;
import java.security.spec.EncodedKeySpec;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.zip.CRC32;

/**
 * Created by PSM on 2014-10-20
 * 用户验证.
 */
public class SynAuth {
    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }


    public String getRoot() {
        return root;
    }

    public void setRoot(String root) {
        this.root = root;
    }


    public long getSessionID() {
        return sessionID;
    }

    public void setSessionID(long sessionID) {
        this.sessionID = sessionID;
    }

    public String getSessionIDHexa8() {
        return sessionIDHexa8;
    }

    public void setSessionIDHexa8(String sessionIDHexa8) {
        this.sessionIDHexa8 = sessionIDHexa8;
    }

    public long getSessionPrivateKey() {
        return sessionPrivateKey;
    }

    public void setSessionPrivateKey(int sessionPrivateKey) {
        this.sessionPrivateKey = sessionPrivateKey;
    }

    public long getSessionTickCountOffset() {
        return sessionTickCountOffset;
    }

    public void setSessionTickCountOffset(long sessionTickCountOffset) {
        this.sessionTickCountOffset = sessionTickCountOffset;
    }

    public long getLastSessionTickCount() {
        return lastSessionTickCount;
    }

    public void setLastSessionTickCount(int lastSessionTickCount) {
        this.lastSessionTickCount = lastSessionTickCount;
    }

    public String getPasswordHashHexa() {
        return passwordHashHexa;
    }

    public void setPasswordHashHexa(String passwordHashHexa) {
        this.passwordHashHexa = passwordHashHexa;
    }

    public long getServerTimeStampOffset() {
        return serverTimeStampOffset;
    }

    public void setServerTimeStampOffset(long serverTimeStampOffset) {
        this.serverTimeStampOffset = serverTimeStampOffset;
    }


    private long sessionID = 0;
    private String sessionIDHexa8 =  "";
    private  long sessionPrivateKey  = 0;
    private  long sessionTickCountOffset = 0;
    private  long lastSessionTickCount = 0;
    private String passwordHashHexa = "";
    private long serverTimeStampOffset = 0;
    private String user = "";
    private String root =  "";
    /*
           字节转换为二进制
        */
    private  String byteToHex(byte[] bts) {
        String des = "";
        String tmp = null;
        for (int i = 0; i < bts.length; i++) {
            tmp = (Integer.toHexString(bts[i] & 0xFF));
            if (tmp.length() == 1) {
                des += "0";
            }
            des += tmp;
        }
        return des;
    }


    /**
     * 获取客户端随机值进行 SHA-256编码
     * @return
     */

    private String SHA256(String aNonce) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            //将字符串转换为byte类型
            byte[] bytes = aNonce.getBytes();
            //编码加密
            md.update(bytes);
            return byteToHex(md.digest());
        }catch (java.security.NoSuchAlgorithmException err) {
            Log.e("警告", "SHA-256编码算法为实现");
        }
        return "";

    }

    public String gotTimestamp(String timestamp) {
        long _timestamp = Long.parseLong(timestamp);
        String clientTime = "";
        String s = "";
        Calendar cal = Calendar.getInstance();
        int _temp  = cal.get(cal.YEAR);
        s = Integer.toBinaryString(_temp);
        if(s.length() < 13 )
            s += "0" + s;
        clientTime = s;

        _temp  = cal.get(cal.MONTH);
        s = Integer.toBinaryString(_temp);
        while(s.length() < 4) {
            s += "0" + s;
        }
        clientTime += s;
        s = Integer.toBinaryString(cal.get(cal.DAY_OF_MONTH) - 1);
        while(s.length() < 5) {
            s += "0" + s;
        }
        clientTime += s;

        s = Integer.toBinaryString(cal.get(cal.HOUR_OF_DAY));

        while(s.length() < 5) { s = '0'+s; }
        clientTime = clientTime +s;

        s = Integer.toBinaryString(cal.get(cal.MINUTE));
        while(s.length() < 6) { s = '0'+s;}
        clientTime = clientTime +s;
        s = s = Integer.toBinaryString(cal.get(cal.SECOND));
        while(s.length() < 6) { s = '0'+s;}
        clientTime = clientTime +s;
        setServerTimeStampOffset(_timestamp - Long.valueOf(clientTime, 2));
        return String.valueOf(serverTimeStampOffset);
    }


    public SynAuth(String root, String username, String password){
        setRoot(root);
        setUser(username);
        setPasswordHashHexa(SHA256( "salt" + password));
    }

    public String doAuth(String serverUrl) throws  Exception{
        HttpUtils http =  new HttpUtils(serverUrl  + "/" + this.root +"/");
        String timeStamp =  http.doGet("TimeStamp", null);

        Log.i("gotTimestamp", gotTimestamp(timeStamp));
        HashMap<String, String> userInfo = new HashMap<String, String>(1);
        userInfo.put("UserName", this.user);
        return  http.doGet("auth", userInfo);
    }

    /**
     * 获取一个客户端时间随机值
     * @return
     */
    public String gotClientNonce () {
        //create client nonce
        String aClientNonce = "", s = "";
        Calendar calendar =Calendar.getInstance();
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        aClientNonce = df.format(calendar.getTime());

        aClientNonce = SHA256(aClientNonce);
        return aClientNonce;
    }

    public boolean doLogin(String serviceUrl) {
        JSONObject tokenObject = null;
        String token = "";
        try {
            tokenObject = new JSONObject(doAuth(serviceUrl));
              if(tokenObject.get("result") == null)
                return false;
            token  =  tokenObject.getString("result");

        }catch (Exception err) {
            err.printStackTrace();
        }
        if(token == "")
             return false;

        String  aNonce =  gotClientNonce();
        HttpUtils http =  new HttpUtils(serviceUrl  + "/");
        HashMap params = new HashMap<String, String>();
        params.put("UserName", user);
        //二次编码
        params.put("Password", SHA256(root + token + aNonce + user +passwordHashHexa));
        params.put("ClientNonce", aNonce);
        //认证 如果成功返回SessionKey ,失败403
        String result =  http.doGet("root/auth", params);

        try {
            JSONObject session = new JSONObject(result);
            //如果成功 返回SessionKey
          Log.i("result", session.getString("result"));
            gotSession(session.getString("result"));

        }catch (JSONException err) {
            err.printStackTrace();
        }

        //TEST
        result =  http.doGet(sessionSign("root/Test"), null);
        Log.i("Response", result);
        return false;
    }


    private String gotSession(String sessionKey) {
       int i = sessionKey.indexOf('+');
       sessionID = Long.valueOf(sessionKey.substring(0, i));
       sessionIDHexa8 = Long.toHexString(sessionID);
        //小于8位补齐
       while(sessionIDHexa8.length() < 8) {
           sessionIDHexa8 = "0" + sessionIDHexa8;
       }
      long r =   getCRC32(sessionKey, 0);
      sessionPrivateKey =
              getCRC32(passwordHashHexa, r);

      return  String.valueOf(sessionPrivateKey);
       //私有密匙

    }

    public String sessionSign(String url) {
        long Tix;
        Date  d = new Date();
        Tix = d.getTime();
        if(lastSessionTickCount == Tix)
            Tix += 1;

        lastSessionTickCount = Tix;

        String aNonce = Long.toHexString(Tix);
        while(aNonce.length() < 8)
             aNonce = '0' +aNonce;
        if (aNonce.length() > 8)
            aNonce = aNonce.substring(aNonce.length() - 8, aNonce.length());

        String ss = Long.toHexString(getCRC32(url,
                getCRC32(aNonce, sessionPrivateKey)));

        while(ss.length() < 8) { ss = '0'+ss; }

        String s = url.indexOf("?") == -1 ? url+ "?session_signature=" : url+ "&session_signature=";
        return s + sessionIDHexa8 + aNonce + ss;
    }

    public long Crc32Add(long crc, int c) {
        int[] table = {
                0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f, 0xe963a535, 0x9e6495a3,
                0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988, 0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91,
                0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
                0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9, 0xfa0f3d63, 0x8d080df5,
                0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
                0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
                0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599, 0xb8bda50f,
                0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924, 0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,
                0x76dc4190, 0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
                0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
                0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457,
                0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
                0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb,
                0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9,
                0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
                0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad,
                0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683,
                0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
                0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb, 0x196c3671, 0x6e6b06e7,
                0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
                0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
                0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef, 0x4669be79,
                0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236, 0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f,
                0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
                0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
                0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21,
                0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
                0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45,
                0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db,
                0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
                0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693, 0x54de5729, 0x23d967bf,
                0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d,
        };

        return  table[(int)((crc^c) & 0xFF) ]^((crc>>8)&0xFFFFFF) ;
    }

    public  long getCRC32(String str, long crc){

        byte[] bytes = str.getBytes();
        crc = crc^0xFFFFFFFF;
         if (crc < 0) {
             crc = (4294967296L + crc);
         }

        for (int n=0; n< str.length(); n++)
        {
            crc= Crc32Add(crc, str.codePointAt(n));
        }
        crc = crc^0xFFFFFFFF;
        if (crc < 0) {
            crc = (4294967296L + crc);
        }
        return crc;
    } 
}
