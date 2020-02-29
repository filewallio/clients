package org.filewall;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import org.apache.commons.io.FilenameUtils;


/**
 * Client class for uploading a file to the Filewall server.<br>
 * To run the Filewall.jar file: java -jar Filewall.jar &lt;args&gt;
 * @author Nigel Patching
 * @version 1.0
 */
public class Filewall {
   
    private String apikey = null;
    private static Logger logger = null;
    
    /**
     * @param apikey The api key supplied at the command line for contacting the server.
     */

    public Filewall(String apikey) {
        this.apikey = apikey;
    }
   
    /** 
     * Manages the process of authorizing, uploading, polling and downloading the converted file contents.
     * @param sourceFileName The full filepath to file being uploaded.
     * @param sourceContent The content of the file being uploaded.
     * @return {@link Result Result} object containing a boolean representing the outcome of the upload
     * and, if succesful, the contents of the returned file.
     */
    public Result convert(String sourceFileName, byte[] sourceContent) {
        JsonObject response = authorize();
        if (response.has("error")) {
            return new Result(false, (response.get("error")).getAsString());            
        }
        
        JsonObject links = (JsonObject)response.get("links");
        String upload = (links.get("upload")).getAsString();        
        boolean success = upload(upload, sourceFileName, sourceContent);        
        if (!success){
            return new Result(false, "upload failed");
        }

        String self = (links.get("self")).getAsString();
        Result pollResult = poll(self);
        if (!pollResult.getbool()) {
            return new Result(false, pollResult.getTxt());
        }

        return download(pollResult.getTxt());
    }

    /**
     * Contacts the server with the apikey.
     * @return JsonObject containing information about the outcome of the authorization and, 
     * if successful, the URL for the upload.
     */
    private JsonObject authorize() {
        logger.log(Level.INFO, "Authorize");
        URL url = null;
        BufferedReader in = null;
        StringBuffer content = null;
        HttpURLConnection conn = null;
        try {
            url = new URL("https://filewall.io/api/authorize");
            conn = (HttpURLConnection) url.openConnection(); 
            conn.setRequestMethod("POST");
            conn.setRequestProperty("APIKEY", this.apikey);
            in = new BufferedReader(new InputStreamReader(conn.getInputStream()));        
            String inputLine;
            content = new StringBuffer();
            while ((inputLine = in.readLine()) != null) {
                content.append(inputLine);
            }
        }
        catch(IOException e){            
            logger.log(Level.SEVERE, "Exception thrown opening connection", e);
            JsonObject result = new JsonObject();
            result.addProperty("error", "unknown_error");
            return result;
        }
        finally {
            if (in != null){
                try {
                    in.close();
                } catch(IOException e) {
                    logger.log(Level.SEVERE, "Exception thrown closing connection", e);
                    JsonObject errorResult = new JsonObject();
                    errorResult.addProperty("error", "unknown_error");
                    return errorResult;
                 }
            }
            if (conn != null){
                conn.disconnect();
            }
        }         
        JsonParser parser = new JsonParser();
        JsonObject result = null;
        JsonElement jsonTree = parser.parse(content.toString());
        if(jsonTree.isJsonObject()) {
            result = jsonTree.getAsJsonObject();
        }
        return result;
    }

    /**
     * Uploads the file content to the server.
     * @param uploadUrl The URL for the upload, returned by the authorize method.
     * @param filename The name of the file being uploaded.
     * @param content The content of the uploaded file.
     * @return boolean representing the outcome of the upload.
     */
    private boolean upload(String uploadUrl, String filename, byte[] content) {
        logger.log(Level.INFO, "Upload");        
        URL url = null;
        HttpURLConnection conn = null;
        int statusCode = 0;
        try  {
            url = new URL(uploadUrl);
            conn = (HttpURLConnection) url.openConnection(); 
            conn.setDoOutput(true);
            conn.setRequestMethod("POST");
            conn.setRequestProperty("filename", filename);
            conn.getOutputStream().write(content);
            statusCode = conn.getResponseCode();
        }
        catch(IOException e){
            logger.log(Level.SEVERE, "Exception thrown opening connection", e);
            return false;
        }
        finally {
            if (conn != null){
                conn.disconnect();
            }
        }   
        return statusCode == 202;
    }

    /**
     * Polls the server with the apikey until the file conversion is completed, 
     * then receives the URL where the converted file is available for download.
     * @param itemUrl The URL containing the api call for downloading the converted file.
     * @return Result object which contains a boolean representing the outcome of the polling
     * and, if successful, the URL for the download location.
     */
    private Result poll(String itemUrl){
        for (int i = 1; i <= 100; i++) {
            logger.log(Level.INFO, "Waiting for result");

            URL url = null;
            BufferedReader in = null;
            StringBuffer content = null;
            HttpURLConnection conn = null;
            try {
                url = new URL(itemUrl);
                conn = (HttpURLConnection) url.openConnection(); 
                conn.setRequestMethod("GET");
                conn.setRequestProperty("APIKEY", this.apikey);
                in = new BufferedReader(new InputStreamReader(conn.getInputStream()));        
                String inputLine;
                content = new StringBuffer();
                while ((inputLine = in.readLine()) != null) {
                    content.append(inputLine);                
                }
            }
            catch(IOException e) {
                logger.log(Level.SEVERE, "Exception thrown opening connection", e);
                return new Result(false, "connection error");
            }
            finally {
                if (in != null){
                    try {
                        in.close();
                    } catch(IOException e) {
                        logger.log(Level.SEVERE, "Exception thrown closing connection", e);
                        return new Result(false, "connection error");
                    }
                }
                if (conn != null){
                    conn.disconnect();
                }
            } 
            JsonParser parser = new JsonParser();
            JsonObject response = null;
            JsonElement jsonTree = parser.parse(content.toString());
            if(jsonTree.isJsonObject()) {
                response = jsonTree.getAsJsonObject();
            }
            
            if (response.has("error")) {
                return new Result(false, (response.get("error")).getAsString());                            
            }
            if (response.has("status")){
                String status = (response.get("status").getAsString()); 
                if (!status.equals("waiting") && !status.equals("processing")) {

                    if (status.equals("failed")){
                        return new Result(false, (response.get("processing_failed")).getAsString());             
                    }

                    if (status.equals("archived")){
                        return new Result(false, (response.get("file_archived")).getAsString());              
                    }

                    if (status.equals("finished")) {
                        if (response.has("links") && ((JsonObject)response.get("links")).has("download")) {
                            return new Result(true, ((JsonObject)response.get("links")).get("download").getAsString());                                
                        }
                        else {
                            return new Result(false, "no_download_url");              
                        }
                    }
                }
            }
            try {
                TimeUnit.SECONDS.sleep(3);
            }
            catch (InterruptedException e) {
                logger.log(Level.SEVERE, "Exception thrown during sleep", e);
                return new Result(false, "error while polling for download");
            }
        }
        return new Result(false, "timeout while polling for download");
    }
    /**
     * Downloads the converted file from the server.
     * @param downloadUrl The URL location for the converted file containing the api call.
     * @return Result object containing a boolean representing the outcome of the download
     * and, if succesful, the contents of the converted file.
     */
    private Result download(String downloadUrl) {
        logger.log(Level.INFO, "Download results");
        URL url = null;        
        HttpURLConnection conn = null;
        int statusCode = 0;
        String header = null;
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        InputStream in = null;
        try {
            url = new URL(downloadUrl);
            conn = (HttpURLConnection) url.openConnection(); 
            conn.setRequestMethod("GET");
            statusCode = conn.getResponseCode();
            header = conn.getHeaderField("content-disposition");
            in = conn.getInputStream();
            byte[] buffer = new byte[4096];
            int n;            
            while ((n = in.read(buffer)) > 0) {
            output.write(buffer, 0, n);
            }
        }
        
        catch(IOException e){
            logger.log(Level.SEVERE, "Exception thrown opening connection", e);
            Result result = new Result(false, "download_failed");
            return result;
        }

        finally {
            if (in != null){
                try {
                    in.close();
                } catch(IOException e) {
                    logger.log(Level.SEVERE, "Exception thrown closing connection", e);
                    Result result = new Result(false, "download_failed");
                    return result;
                }
            }
            if (conn != null) {
                conn.disconnect();
            }
        }
        
        byte[] content = output.toByteArray();
        
        if (statusCode != 200) {
            logger.log(Level.SEVERE, "download failed");
            Result result = new Result(false, "download_failed");
            return result;
        }

        String[] tokens = header.split("; filename=");
        String filename = tokens[1].replace("\"" , "");

        logger.log(Level.INFO, "Secured file " +filename  +" (" +content.length +" bytes) downloaded");
        
        Result result = new Result(true, filename, content);        
        return result;
    }

    /**
     * Prints the command line arguments requred to use the client.
     */
    public static void printHelp() {
        System.out.println("filewall.py -v <apikey> <source_file>");
        System.out.println("    -v            : verbose (optional)");
        System.out.println("    apikey        : your apikey");
        System.out.println("    source_file   : local file");
    }

    /**
     * Checks the file exists on the client machine, creates an instance of the class,
     * and then calls the {@link #convert(String, byte[]) convert} method.
     * @param args -v (optional), apikey, source file name
     */
    public static void main(String[] args) {
        logger = Logger.getLogger(Filewall.class.getName());
        FileHandler fh = null;
        try {
            fh = new FileHandler("Filewall.log");
        }
        catch (IOException e){
            System.out.println("Exception thrown trying to open Filewall.log");
        }
        logger.addHandler(fh);
        fh.setFormatter(new SimpleFormatter());

        if (args.length < 2 || args[0] == "help") {
            printHelp();
            System.exit(1);
        }

        if (args[0] == "-v") {
            logger.setLevel(Level.INFO);
        }

        String apikey = args[args.length-2];
        String sourceFilepath = args[args.length-1];
        
        File sourceFile = new File(sourceFilepath);
        if (!sourceFile.exists()) {
            System.out.printf("File %s does not exist", sourceFilepath);
            System.exit(1);
        }
        
        String sourceDirPath = sourceFile.getParent();
        String sourceFileName = sourceFile.getName();
        
        byte[] sourceContent = new byte[0];
        try {
             sourceContent = Files.readAllBytes(Paths.get(sourceFilepath));
        }
        catch (IOException e){
            logger.log(Level.SEVERE, "error reading file", e);
            System.out.printf("Error reading file %s", sourceFileName);
            System.exit(1);
        }     

        Result result = new Filewall(apikey).convert(sourceFileName, sourceContent);
    
        if (!result.getbool()) {
            System.out.println("Error: " +result.getTxt());
            System.exit(1);
        }
        
        String resultFileName = result.getTxt();
        byte[] resultContent = result.getContent();

        int i=1;        
        File outputFile = new File(sourceDirPath, resultFileName);
        
        while (outputFile.exists()) {
            String filenameNoExt = FilenameUtils.removeExtension(resultFileName);
            String ext = FilenameUtils.getExtension(resultFileName);
            String newFileName = filenameNoExt +'_' +i +"." +ext;
            outputFile = new File(sourceDirPath, newFileName);
            i++;
        }

        try {
            FileOutputStream out = new FileOutputStream(outputFile);
            out.write(resultContent);
            out.flush();
            out.close();
        }
        catch (IOException e) {
            logger.log(Level.SEVERE, "error writing file", e);
            System.out.printf("Error writing file %s", outputFile.getAbsolutePath());
            System.exit(1);
        }
        System.out.println("Secure result: " +outputFile.getAbsolutePath());    
    }
    
    /**
     * An object containing information generated after an interaction with the server.
     */
    private class Result {
        
        private boolean bool = false;
        private String txt = null;
        private byte[] content = new byte[0];

        public Result(boolean bool, String txt) {
            this(bool, txt, new byte[0]);
        }

        /**
         * @param bool Represents the success of the interaction.
         * @param txt Contains a JSON object generated by the server.
         * @param content The contents of the converted file being downloaded.
         */
        public Result(boolean bool, String txt, byte[] content) {
            this.bool = bool;
            this.txt = txt;
            this.content = content;
        }
        
        public boolean getbool() {
            return bool;
        }

        public String getTxt() {
            return txt;
        }

        public byte[] getContent() {
            return content;
        }
    }
}