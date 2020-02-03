<?php

//Check for verbose flag
$loggerLevel = isset($argv[1]) && $argv[1] == "-v" ? 1 : 0;
define("LOGGER",$loggerLevel);

// Calculate indexes (If LOGGER == 1 then verbose is activated)
$apikey_index = LOGGER ? 2 : 1;
$sourceFile_index = LOGGER ? 3 : 2;

//Getting parameters
$apikey = isset($argv[$apikey_index]) ? $argv[$apikey_index] : "";
$source_file = isset($argv[$sourceFile_index]) ? $argv[$sourceFile_index] : "";

if(empty($apikey) || empty($source_file) )
{
        print("filewall.py -v <apikey> <source_file> <replace_source>\n");
        print("    -v            : verbose (optional)\n");
        print("    apikey        : your apikey\n");
        print("    source_file   : local file\n");
        exit;
}

if(!is_file($source_file)){
        exit("'$source_file' is not a file\n");
}

if(!file_exists($source_file)){
        exit("File '$source_file' does not exists\n");
}

$filewall = new Filewall($apikey);

$file_content = file_get_contents($source_file);
$file_name = basename($source_file);
$file_path = dirname(realpath($source_file));

$result = $filewall->convert($file_name, $file_content,$file_path);

//Print in case of an error
if(!isset($result['success'])) print_r($result);




Class Filewall
{
        private $apikey;

        public function __construct($apikey){
                $this->apikey = $apikey;
        }

        public function convert($filename, $file_content,$file_path){

                $response = $this->authorize();
                if(isset($response['output']['error'])){
                        return $response;
                }

                //Uploading file (No response is exptected)
                $this->upload($response["output"]["links"]["upload"], $filename, $file_content);

                //Request for downloading file
                $response = $this->poll($response["output"]["links"]["self"]);
                if(isset($response['success'])){
                        $this->download($response['success'],$file_path."/".$response['filename']);
                }

                return $response;

        }

        private function authorize(){
                $this->logger("Authorize");
                return $this->postRequest("https://filewall.io/api/authorize", array("APIKEY: ".$this->apikey) );
        }

        private function upload($url,$filename,$file_content){
                $this->logger("Upload");
                $response = $this->postRequest($url, array("filename: ". $filename) ,$file_content);
                if(isset($response['http_code']) && $response['http_code'] != 202){
                        return array("error" => "upload_fail");
                }

                return $response;
        }
		
		
        private function poll($url){
                $this->logger("Waiting for result");

                $i = 1 ;
                while($i < 100)
                {
                        $response = $this->postRequest($url, array("APIKEY: ". $this->apikey));
                        if(isset($response['output']['error'])){
                                return array('error' => $response['output']['error']);
                        }

                        if(isset($response['output']['status']))
                        {
                                if(!in_array( $response['output']['status'], array( "waiting", "processing") )){
                                        if( $response['output']["status"] == "failed"){
                                                return array("error" => "processing_failed");
                                        }

                                }

                                if( $response['output']["status"] == "archived"){
                                        return array("error" => "file_archived");
                                }


                                if( $response['output']["status"] == "finished"){
                                        if( isset($response['output']["links"]) && isset($response['output']["links"]["download"]) ){
                                                return array("success" => $response['output']["links"]["download"], "filename" => $response['output']['name']);
                                        }
                                        else{
                                                return array("error" => "no_download_url");
                                        }

                                }

                        }


                        sleep(3);
                        $i++;
                }
        }
								

        private function download($link,$filename){
                echo "Secure result: $filename";
				
                file_put_contents($filename,file_get_contents($link));
        }

        private function logger($string){
                if(LOGGER) echo $string."\n";
        }


        //Send HTTP Request with APIKEY as header
        private function postRequest($url,$headers, $content ="")
        {
                $ch = curl_init();
                curl_setopt($ch, CURLOPT_URL, $url);
                curl_setopt($ch, CURLOPT_HTTPHEADER,$headers);
                curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
                if(!empty($content)){
                        curl_setopt($ch, CURLOPT_POST,           1 );
                        curl_setopt($ch, CURLOPT_POSTFIELDS,     $content );
                }

                $output = curl_exec($ch);
                $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);

                $response["http_code"] = $http_code;
                $response["output"] = json_decode($output,true);

                curl_close($ch);

                return $response;
        }


}


?>
								