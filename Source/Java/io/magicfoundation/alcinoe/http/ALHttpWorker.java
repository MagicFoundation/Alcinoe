package io.magicfoundation.alcinoe.http;

import io.magicfoundation.alcinoe.broadcastreceiver.ALBroadcastReceiver;
import android.content.Context;
import android.util.Log;
import android.os.Build;
import android.content.Intent;
import android.content.SharedPreferences;
import androidx.annotation.NonNull;
import androidx.work.Constraints;
import androidx.work.Data;
import androidx.work.NetworkType;
import androidx.work.OneTimeWorkRequest;
import androidx.work.Worker;
import androidx.work.WorkerParameters;
import androidx.work.WorkManager;
import androidx.work.OutOfQuotaPolicy;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.net.URL;
import java.net.URI;
import java.util.UUID;

/**
 * Generic background HTTP worker for uploading or downloading data.
 */
public class ALHttpWorker extends Worker {

  public static final String ACTION_HTTP_COMPLETED = "io.magicfoundation.alcinoe.http.action.HTTP_COMPLETED";
  public static final String EXTRA_HTTP_SUCCESS = "io.magicfoundation.alcinoe.http.extra.HTTP_SUCCESS";
  public static final String EXTRA_HTTP_CANCELED = "io.magicfoundation.alcinoe.http.extra.HTTP_CANCELED";
  public static final String EXTRA_HTTP_REQUEST_ID = "io.magicfoundation.alcinoe.http.extra.HTTP_REQUEST_ID";
  public static final String EXTRA_HTTP_RESPONSE_STATUS_CODE = "io.magicfoundation.alcinoe.http.extra.HTTP_RESPONSE_STATUS_CODE";
  public static final String EXTRA_HTTP_RESPONSE_HEADERS = "io.magicfoundation.alcinoe.http.extra.HTTP_RESPONSE_HEADERS";
  public static final String EXTRA_HTTP_RESPONSE_BODY_FILE_PATH = "io.magicfoundation.alcinoe.http.extra.HTTP_RESPONSE_BODY_FILE_PATH";

  private static final String PREF_NAME = "ALHttpWorker";
  private static final String TAG = "ALHttpWorker";
  private static final String KEY_REQUEST_BODY_FILE_PATH = "request_body_file_path";
  private static final String KEY_DELETE_REQUEST_BODY_FILE = "delete_request_body_file";
  private static final String KEY_REQUEST_BODY_STRING = "request_body_string";
  private static final String KEY_REQUEST_URL = "request_url";
  private static final String KEY_REQUEST_METHOD = "request_method";
  private static final String KEY_REQUEST_HEADERS = "request_headers";
  private static final String KEY_ENQUEUE_TIME = "enqueue_time";
  private static final long MAX_AGE_MS = 7L * 24L * 60L * 60L * 1000L; // 7 days in milliseconds
  private static final long ONE_DAY_MS = 24L * 60L * 60L * 1000L; // 1 day in milliseconds
  private static final int BUFFER_SIZE = 64 * 1024; // 64 KB
  
  private static volatile long sLastCleanupTime = 0L;

  public ALHttpWorker(
          @NonNull Context context,
          @NonNull WorkerParameters params) {
    super(context, params);
  }

  @NonNull
  @Override
  public Result doWork() {    

    boolean canceled = false;
    boolean deleteRequestBodyFile = false;
    File requestBodyFile = null;
    File responseBodyFile = null;
    try {
    
      /* Extract input parameters from WorkManager */
      Data inputData = getInputData();
      String requestBodyFilePath = inputData.getString(KEY_REQUEST_BODY_FILE_PATH);
      deleteRequestBodyFile = inputData.getBoolean(KEY_DELETE_REQUEST_BODY_FILE, false);
      String requestBodyString = inputData.getString(KEY_REQUEST_BODY_STRING);
      String requestUrl = inputData.getString(KEY_REQUEST_URL);
      String requestMethod = inputData.getString(KEY_REQUEST_METHOD);
      String requestHeaders = inputData.getString(KEY_REQUEST_HEADERS);
      long enqueueTime = inputData.getLong(KEY_ENQUEUE_TIME, -1L);

      /* Initialize the request body file if provided */
      if (requestBodyFilePath != null && !requestBodyFilePath.isEmpty()) {
        requestBodyFile = new File(requestBodyFilePath);
        if (!requestBodyFile.exists() || !requestBodyFile.isFile()) 
          throw new IllegalArgumentException("body file does not exist (" + requestBodyFilePath + ")");
      }
          
      /* Abort if URL is missing */
      if (requestUrl == null || requestUrl.isEmpty())
        throw new IllegalArgumentException("URL is missing");

      /* Check if this work has been canceled by client before starting HTTP */
      cleanupExpiredCancellationFlags();
      if (isCanceled()) {
        canceled = true;
        throw new IllegalStateException("HTTP request \"" + getId().toString() + "\" has been canceled; skipping execution.");  
      }  

      /* Abort if request has been queued for too long (expired) */
      if ((enqueueTime > 0) && (System.currentTimeMillis() - enqueueTime > MAX_AGE_MS))
        throw new IllegalStateException("work request expired (enqueued too long ago)");
      
      /* Default to POST if method not specified */
      if (requestMethod == null || requestMethod.isEmpty()) requestMethod = "POST";

      /* Variables used for connection and response */
      HttpURLConnection connection = null;

      try {
        
        /* Create the response file before sending the request (so the request isn't sent if we fail creating the file) */
        responseBodyFile = File.createTempFile("ALHttpWorker_", ".tmp", getApplicationContext().getCacheDir()); /* Throws IOException If a file could not be created */
        String responseBodyFilePath = responseBodyFile.getAbsolutePath();

        /* Open HTTP connection */
        URI uri = URI.create(requestUrl);
        URL url = uri.toURL(); /* Throws MalformedURLException If a protocol handler for the URL could not be found, or if some other error occurred while constructing the URL. */
        connection = (HttpURLConnection) url.openConnection(); /* Throws IOException if an I/O exception occurs. */

        /* Configure connection */
        boolean doOutput = (requestBodyFile != null || (requestBodyString != null && !requestBodyString.isEmpty()));
        connection.setConnectTimeout(60_000);
        connection.setReadTimeout(180_000);
        connection.setDoOutput(doOutput);
        connection.setRequestMethod(requestMethod);

        /* Apply request headers if any */
        if (requestHeaders != null && !requestHeaders.trim().isEmpty()) {
          String[] lines = requestHeaders.split("\\r?\\n");
          for (String line : lines) {
            if (line == null) continue;
            line = line.trim();
            if (line.isEmpty()) continue;

            int idx = line.indexOf(':');
            if (idx <= 0) {
              Log.w(TAG, "Skipping invalid header line: " + line);
              continue;
            }

            String name  = line.substring(0, idx).trim();
            String value = line.substring(idx + 1).trim();

            if (!name.isEmpty()) 
              connection.setRequestProperty(name, value);
          }
        }
       
        /* Send request body (string or file) if present */
        if (doOutput) {
          InputStream in = null;
          OutputStream out = null;
          try {
            
            long requestContentLength;
            if (requestBodyFile != null) {
              requestContentLength = requestBodyFile.length();
              in = new BufferedInputStream(new FileInputStream(requestBodyFile)); 
            } 
            else {
              byte[] bytes = requestBodyString.getBytes(StandardCharsets.UTF_8);
              requestContentLength = bytes.length;
              in = new ByteArrayInputStream(bytes);
            }
            
            connection.setFixedLengthStreamingMode(requestContentLength);
            connection.connect();
            
            out = new BufferedOutputStream(connection.getOutputStream());
            byte[] buffer = new byte[BUFFER_SIZE];
            int read;
            while ((read = in.read(buffer)) != -1) out.write(buffer, 0, read);
            out.flush();
            
          } finally {
            if (in != null) { try { in.close(); } catch (Exception ignored) {} }
            if (out != null) { try { out.close(); } catch (Exception ignored) {} }
          }
        }
        
        /* No request body */
        else {
          connection.connect();
        }
        
         /* Read response headers */
        StringBuilder sb = new StringBuilder();
        for (int i = 1; ; i++) {
          String key = connection.getHeaderFieldKey(i);
          if (key == null) break; // no more headers
          String value = connection.getHeaderField(i);
          if (value != null) sb.append(key).append(": ").append(value).append('\n');
        }
        String responseHeaders = sb.toString();

        /* Read HTTP status code */
        int responseStatusCode = connection.getResponseCode();

        /* Save response body into the responseBodyFile */
        InputStream responseStream = (responseStatusCode >= 400) ? connection.getErrorStream() : connection.getInputStream();
        if (responseStream != null) { 
          InputStream in = null;
          OutputStream out = null;
          try {
            in = new BufferedInputStream(responseStream);
            out = new BufferedOutputStream(new FileOutputStream(responseBodyFile));
            byte[] buffer = new byte[BUFFER_SIZE];
            int read;
            while ((read = in.read(buffer)) != -1) out.write(buffer, 0, read);
            out.flush();
          } finally {
            if (in != null) { try { in.close(); } catch (Exception ignored) {} }
            if (out != null) { try { out.close(); } catch (Exception ignored) {} }
          }
        }
        
        // Successful HTTP 2xx
        if (responseStatusCode >= 200 && responseStatusCode < 300) { 
          Log.d(TAG, "HTTP request \""+getId().toString()+"\" completed successfully (HTTP " + responseStatusCode + ")");
          if (deleteRequestBodyFile && requestBodyFile != null) requestBodyFile.delete();
          clearCancellationFlag();
          sendCompletionBroadcast(true/*success*/, false/*canceled*/, responseStatusCode, responseHeaders, responseBodyFilePath);
          return Result.success(); 
        } 
        // Server-side error (5xx) -> retry
        else if (responseStatusCode >= 500 && responseStatusCode < 600) { 
          Log.w(TAG, "HTTP request \""+getId().toString()+"\" failed with server error, scheduling retry (HTTP " + responseStatusCode + ")");
          if (responseBodyFile != null) responseBodyFile.delete();
          return Result.retry(); 
        } 
        // Client error or any other status -> permanent failure
        else { 
          Log.e(TAG, "HTTP request \""+getId().toString()+"\" failed with non-retriable error (HTTP " + responseStatusCode + ")");
          if (deleteRequestBodyFile && requestBodyFile != null) requestBodyFile.delete();
          clearCancellationFlag();
          sendCompletionBroadcast(false/*success*/, false/*canceled*/, responseStatusCode, responseHeaders, responseBodyFilePath);
          return Result.failure(); 
        }

      } catch (IOException e) {
        Log.w(TAG, "HTTP request \""+getId().toString()+"\" failed with IOException, scheduling retry", e);
        if (responseBodyFile != null) responseBodyFile.delete();
        return Result.retry();

      } finally {
        if (connection != null) { try { connection.disconnect(); } catch (Exception ignored) {} }
      }
 
    } catch (Exception e) {
      if (canceled) Log.d(TAG, "HTTP request \"" + getId().toString() + "\" has been canceled; skipping execution.");
      else Log.e(TAG, "HTTP request \""+getId().toString()+"\" failed with non-retriable error", e);
      if (deleteRequestBodyFile && requestBodyFile != null) requestBodyFile.delete();
      if (responseBodyFile != null) responseBodyFile.delete();
      clearCancellationFlag();
      sendCompletionBroadcast(false/*success*/, canceled/*canceled*/, -1/*responseStatusCode*/, ""/*responseHeaders*/, ""/*responseBodyFilePath*/);
      return Result.failure();
    }

  }
    
  private void sendCompletionBroadcast(
                 boolean success,
                 boolean canceled,
                 int responseStatusCode,
                 String responseHeaders,
                 String responseBodyFilePath) {

    Intent intent = new Intent(ACTION_HTTP_COMPLETED);
    intent.setClass(getApplicationContext(), io.magicfoundation.alcinoe.broadcastreceiver.ALBroadcastReceiver.class);

    intent.putExtra(EXTRA_HTTP_SUCCESS, success);
    intent.putExtra(EXTRA_HTTP_CANCELED, canceled);
    intent.putExtra(EXTRA_HTTP_REQUEST_ID, getId().toString());
    intent.putExtra(EXTRA_HTTP_RESPONSE_STATUS_CODE, responseStatusCode);  
    intent.putExtra(EXTRA_HTTP_RESPONSE_HEADERS, responseHeaders != null ? responseHeaders : "");
    intent.putExtra(EXTRA_HTTP_RESPONSE_BODY_FILE_PATH, responseBodyFilePath != null ? responseBodyFilePath : "");
 
    getApplicationContext().sendBroadcast(intent);

  }

  private static @NonNull UUID enqueue(
                                 @NonNull Context context,
                                 @NonNull String url,
                                 @NonNull String method,
                                 String requestBodyFilePath,
                                 boolean deleteRequestBodyFile,
                                 String requestBodyString,
                                 String headers) {

    Data.Builder dataBuilder = new Data.Builder()
                                     .putString(KEY_REQUEST_URL, url != null ? url : "")
                                     .putString(KEY_REQUEST_METHOD, method != null ? method : "")
                                     .putString(KEY_REQUEST_BODY_FILE_PATH, requestBodyFilePath != null ? requestBodyFilePath : "")
                                     .putBoolean(KEY_DELETE_REQUEST_BODY_FILE, deleteRequestBodyFile)
                                     .putString(KEY_REQUEST_BODY_STRING, requestBodyString != null ? requestBodyString : "")
                                     .putString(KEY_REQUEST_HEADERS, headers != null ? headers : "")
                                     .putLong(KEY_ENQUEUE_TIME, System.currentTimeMillis());
 
    Constraints constraints = new Constraints.Builder()
                                    .setRequiredNetworkType(NetworkType.CONNECTED)
                                    .build();

    OneTimeWorkRequest.Builder builder = new OneTimeWorkRequest.Builder(ALHttpWorker.class)
                                               .setInputData(dataBuilder.build())
                                               .setConstraints(constraints);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) 
      builder.setExpedited(OutOfQuotaPolicy.RUN_AS_NON_EXPEDITED_WORK_REQUEST);

    OneTimeWorkRequest request = builder.build();

    WorkManager.getInstance(context).enqueue(request);

    return request.getId();

  }

  /**
   * Enqueue a background HTTP request using WorkManager.
   *
   * @param context                Android context
   * @param url                    Target HTTP/HTTPS URL
   * @param method                 HTTP method, e.g. "POST" or "PUT"
   * @param requestBodyFilePath    Absolute path to the file to upload
   * @param deleteRequestBodyFile  If {@code true}, the body file will be deleted after the
   *                               work has completed (success or failure)
   * @param headers                Optional HTTP headers as a single string (may be null/empty).
   *                               Format: "Name1: value1\r\nName2: value2\r\n..."
   *
   * @return The request ID associated with this HTTP request.
   */
  public static @NonNull UUID enqueue(
                                @NonNull Context context,
                                @NonNull String url,
                                @NonNull String method,
                                @NonNull String requestBodyFilePath,
                                boolean deleteRequestBodyFile,
                                String headers) {
    return enqueue(
             context, // @NonNull Context context,
             url, // @NonNull String url,
             method, // @NonNull String method,
             requestBodyFilePath, // String requestBodyFilePath,
             deleteRequestBodyFile, // boolean deleteRequestBodyFile,
             null, // String requestBodyString,
             headers); // String headers)
  }
  
  /**
   * Enqueue a background HTTP request using WorkManager.
   *
   * @param context              Android context
   * @param url                  Target HTTP/HTTPS URL
   * @param method               HTTP method, e.g. "POST" or "PUT"
   * @param requestBodyString    Payload to upload (will be sent as UTF-8)
   * @param headers              Optional HTTP headers as a single string (may be null/empty).
   *                             Format: "Name1: value1\r\nName2: value2\r\n..."
   *
   * @return The request ID associated with this HTTP request.
   */
  public static @NonNull UUID enqueue(
                                @NonNull Context context,
                                @NonNull String url,
                                @NonNull String method,
                                @NonNull String requestBodyString,
                                String headers) {
    return enqueue(
             context, // @NonNull Context context,
             url, // @NonNull String url,
             method, // @NonNull String method,
             null, // String requestBodyFilePath,
             false, // boolean deleteRequestBodyFile,
             requestBodyString, // String requestBodyString,
             headers); // String headers)
  }
  
  private boolean isCanceled() {
    SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
    return sharedPref.contains(getId().toString());
  }

  private void clearCancellationFlag() {
    SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
    sharedPref.edit().remove(getId().toString()).apply();
  }
  
  private void cleanupExpiredCancellationFlags() {
    long now = System.currentTimeMillis();
    if (now - sLastCleanupTime < ONE_DAY_MS) return;
    sLastCleanupTime = now;
    try {
      SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
      Map<String, ?> all = sharedPref.getAll();
      if (!all.isEmpty()) {
        SharedPreferences.Editor editor = sharedPref.edit();
        boolean hasChanges = false;
        for (Map.Entry<String, ?> entry : all.entrySet()) {
          Object value = entry.getValue();
          if (value instanceof Long) {
            long timestamp = (Long) value;
            if (now - timestamp > MAX_AGE_MS) {
              editor.remove(entry.getKey());
              hasChanges = true;
            }
          }
        }
        if (hasChanges) editor.apply();
      }     
    } catch (Exception e) {
      Log.e(TAG, "Error while cleaning ALHttpWorker SharedPreferences", e);
      try {
        SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
        sharedPref.edit().clear().apply();
      } catch (Exception ignored) {
        // Swallow secondary failure
        Log.e(TAG, "Error while clearing ALHttpWorker SharedPreferences after previous failure", ignored);
      }  
    }
  }
  
  /**
   * Cancel a previously enqueued HTTP request.
   *
   * @param context   Android context
   * @param requestId The request ID returned by the enqueue(...) methods.
   *
   * Note: This just requests cancellation; the work may still finish if it is already completing.
   */
  public static void cancel(
                      @NonNull Context context,
                      @NonNull UUID requestId) {
    SharedPreferences sharedPref = context.getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
    sharedPref.edit().putLong(requestId.toString(), System.currentTimeMillis()).apply();
  }

}