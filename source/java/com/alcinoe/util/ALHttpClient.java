package com.alcinoe.util;

import java.util.HashMap;
import java.util.Map;
import java.net.URLEncoder;
import java.io.UnsupportedEncodingException;

public class ALHttpClient {

  public static String HTTPEncodeParamNameValues(HashMap<String, String> params) throws UnsupportedEncodingException {
    StringBuilder result = new StringBuilder();
    boolean first = true;
    for(Map.Entry<String, String> entry : params.entrySet()){
      if (first) first = false;
      else result.append("&");    
      result.append(URLEncoder.encode(entry.getKey(), "UTF-8"));
      result.append("=");
      result.append(URLEncoder.encode(entry.getValue(), "UTF-8"));
    }    
    return result.toString();
  }
   
}