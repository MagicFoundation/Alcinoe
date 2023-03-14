package com.alcinoe.location;

import java.util.List;
import android.location.Address;
import org.json.JSONObject;
import org.json.JSONArray;
import java.net.URL;
import java.net.URLEncoder;
import java.net.HttpURLConnection;
import java.io.InputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Locale;
import java.lang.StringBuilder;
import android.util.Log;

public class ALGeocoder {

  public enum ApiServerType {
    GMAPS,
    CUSTOM 
  }

  //when you update this function update also it's equivalent delphi implementation (look for keyword https://maps.googleapis.com/maps/api/geocode/json)
  public static Address getFromLocation (String apiServerURL,
                                         ApiServerType apiServerType, 
                                         double latitude, 
                                         double longitude, 
                                         String language, 
                                         StringBuilder apiStatus) {

    Address address = null;
    try {

      URL url = new URL(apiServerURL+(apiServerURL.indexOf("?") >= 0 ? "&" : "?")+
                          "latlng="+Double.toString(latitude)+","+Double.toString(longitude)+"&"+
                          ((language != null) && (language.length() > 0) ? "language="+URLEncoder.encode(language, "UTF-8") : ""));
      HttpURLConnection httpURLConnection = (HttpURLConnection) url.openConnection(); 
      httpURLConnection.setConnectTimeout(60000);
      httpURLConnection.setReadTimeout(60000);      
      InputStream inputStream = httpURLConnection.getInputStream();
      ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
      byte[] buffer = new byte[1024];
      int length;
      while ((length = inputStream.read(buffer)) != -1) {
          byteArrayOutputStream.write(buffer, 0, length);
      }
      String jsonTxt = byteArrayOutputStream.toString("UTF-8");
      JSONObject jsonObject = new JSONObject(jsonTxt);

      // result can be returned in 2 different formats
      // Google Maps Format : 
      //{
      //   "results" : [
      //      {
      //         "address_components" : [
      //            {
      //               "long_name" : "Toledo",
      //               "short_name" : "Toledo",
      //               "types" : [ "locality", "political" ]
      //            },
      //            {
      //               "long_name" : "Toledo",
      //               "short_name" : "Toledo",
      //               "types" : [ "administrative_area_level_4", "political" ]
      //            },
      //            {
      //               "long_name" : "Vega de Toledo",
      //               "short_name" : "Vega de Toledo",
      //               "types" : [ "administrative_area_level_3", "political" ]
      //            },
      //            {
      //               "long_name" : "Toledo",
      //               "short_name" : "TO",
      //               "types" : [ "administrative_area_level_2", "political" ]
      //            },
      //            {
      //               "long_name" : "Castile-La Mancha",
      //               "short_name" : "CM",
      //               "types" : [ "administrative_area_level_1", "political" ]
      //            },
      //            {
      //               "long_name" : "Spain",
      //               "short_name" : "ES",
      //               "types" : [ "country", "political" ]
      //            }
      //         ],
      //         "formatted_address" : "Toledo, Toledo, Spain",
      //         "geometry" : {
      //            "bounds" : {
      //               "northeast" : {
      //                  "lat" : 39.88605099999999,
      //                  "lng" : -3.9192423
      //               },
      //               "southwest" : {
      //                  "lat" : 39.8383676,
      //                  "lng" : -4.0629256
      //               }
      //            },
      //            "location" : {
      //               "lat" : 39.8628316,
      //               "lng" : -4.027323099999999
      //            },
      //            "location_type" : "APPROXIMATE",
      //            "viewport" : {
      //               "northeast" : {
      //                  "lat" : 39.88605099999999,
      //                  "lng" : -3.9192423
      //               },
      //               "southwest" : {
      //                  "lat" : 39.8383676,
      //                  "lng" : -4.0629256
      //               }
      //            }
      //         },
      //         "place_id" : "ChIJ8f21C60Lag0R_q11auhbf8Y",
      //         "types" : [ "locality", "political" ]
      //      }
      //   ],
      //   "status" : "OK"
      //}
      //
      // Or in our own Format :     
      //{
      //   "stat" : "ok"
      //   "country" : "ES"
      //   "region" : "Castile-La Mancha",
      //   "city" : "Toledo", 
      //   "district" : ""
      //   "zipcode" : ""
      //}
      //
      
      String country = "";
      String region = "";
      String city = "";
      String district = "";
      String zipcode = "";
     
      ///////////
      // GMAPS //
      ///////////

      if (apiServerType == ApiServerType.GMAPS) {
        String status = jsonObject.optString("status");
        if ((status != null) && (!status.isEmpty())) {
          
          if (apiStatus != null) { apiStatus.append(status); }
          if (status.equalsIgnoreCase("OK")) {
            
            JSONArray results = jsonObject.getJSONArray("results"); 
            if (results.length() > 0) {
                          
              JSONArray addressComponents = results.getJSONObject(0).getJSONArray("address_components");
              for(int i=addressComponents.length() - 1; i >= 0; i--) {
                
                JSONObject addressComponent = addressComponents.getJSONObject(i);
                JSONArray types = addressComponent.getJSONArray("types");
                ArrayList<String> typesArr = new ArrayList<String>();     
                for (int j=0; j < types.length(); j++) { 
                  typesArr.add(types.getString(j));
                } 
                
                //if country
                if ((country.equals("")) && (typesArr.contains("country"))) { country = addressComponent.getString("short_name"); }

                //if administrative_area_level_1
                else if ((region.equals("")) && (typesArr.contains("administrative_area_level_1"))) { region = addressComponent.getString("long_name"); }

                //if locality
                else if ((city.equals("")) && ((typesArr.contains("locality")) ||
                                               (typesArr.contains("administrative_area_level_3")))) { city = addressComponent.getString("long_name"); }

                //if neighborhood / sublocality
                else if  ((district.equals("")) && ((typesArr.contains("neighborhood")) ||
                                                    (typesArr.contains("sublocality"))))  { district = addressComponent.getString("long_name"); }

                //if postal_code
                else if ((zipcode.equals("")) && (typesArr.contains("postal_code"))) { zipcode = addressComponent.getString("long_name"); }

              }
              
              //init the result
              if ((!country.equals("")) ||
                  (!region.equals("")) ||
                  (!city.equals("")) ||
                  (!district.equals("")) ||
                  (!zipcode.equals(""))) {
                  
                address = new Address(new Locale(language));     
                address.setLatitude(latitude);
                address.setLongitude(longitude);
                address.setCountryCode(country);
                address.setAdminArea(region);
                address.setLocality(city);
                address.setSubLocality(district);
                address.setPostalCode(zipcode);
                                      
              }
            
            }
            
          }

        }
      }

      ////////////
      // CUSTOM //
      ////////////
      
      else {
        
        String status = jsonObject.optString("stat");
        if ((status != null) && (!status.isEmpty())) {
        
          if (apiStatus != null) { apiStatus.append(status); }
          if (status.equalsIgnoreCase("ok")) {
          
            country = jsonObject.optString("country");
            region = jsonObject.optString("region");
            city = jsonObject.optString("city");
            district = jsonObject.optString("district");
            zipcode = jsonObject.optString("zipcode");
                
            //init the result
            if ((!country.equals("")) ||
                (!region.equals("")) ||
                (!city.equals("")) ||
                (!district.equals("")) ||
                (!zipcode.equals(""))) {
                
              address = new Address(new Locale(language));     
              address.setLatitude(latitude);
              address.setLongitude(longitude);
              address.setCountryCode(country);
              address.setAdminArea(region);
              address.setLocality(city);
              address.setSubLocality(district);
              address.setPostalCode(zipcode);
                                    
            }

          }  
          
        }      
        
      }
           
    } 
    catch (Throwable e){ 
      Log.e("ALGeocoder", "getFromLocation - Exception", e); 
      address = null;
    }  
  
    return address;
    
  }
  
  public static Address getFromLocation (double latitude, 
                                         double longitude, 
                                         String language, 
                                         String apiKey, /* Google maps api key */
                                         StringBuilder apiStatus) {
    try {
      return getFromLocation ("https://maps.googleapis.com/maps/api/geocode/json?key="+URLEncoder.encode(apiKey, "UTF-8"),
                              ApiServerType.GMAPS,
                              latitude, 
                              longitude, 
                              language, 
                              apiStatus);
    }
    catch (Throwable e){ 
      Log.e("ALGeocoder", "getFromLocation - Exception", e); 
      return null;
    }  
  }

}