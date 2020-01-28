<?php

if(isset($argv[1]) && $argv[1] == "-v"){
        $apikey = isset($argv[2]) ? $argv[2] : "";
        $source_file = isset($argv[3]) ? $argv[3] : "";
}
else{
    print("filewall.py -v <apikey> <source_file> <replace_source>\n");
    print("    -v            : verbose (optional)\n");
    print("    apikey        : your apikey\n");
    print("    source_file   : local file\n");
    exit;
}

if(!file_exists($source_file))
{
        exit("File $source_file does not exists\n");
}

//Building FIlewall Object
$filewall = new Filewall($apikey);

$file_name = $source_file;
$file_content = file_get_contents($file_name); //Getting file data
$filewall->convert($file_name, $file_content);





Class Filewall
{
        private $apikey;
        public function __construct($apikey){
                $this->apikey = $apikey;
        }

        public function convert($filename, $file_content){

                $response = $this->authorize();
                if(isset($response['error'])){
                        return $response;
                }

                //Uploading file (No response is exptected)
                $this->upload($response["links"]["upload"], $filename, $file_content);

                //Request for downloading file
                $response = $this->poll($response["links"]["self"]);
                if(isset($response['success'])){
                        $this->download($response['success'],$filename);
                }



        }

        private function authorize(){
                echo "Authorize\n";
                return $this->postRequest("https://filewall.io/api/authorize", array("APIKEY: ".$this->apikey) );
        }




        private function upload($url,$filename,$file_content){
                echo "Upload\n";
                $response = $this->postRequest($url, array("filename: ". $filename) ,$file_content);
                if(isset($response['error'])){
                        return array("error" => "upload_fail");
                }

                return $response;
        }

        private function poll($url){
                echo "Waiting for result";

                $i = 1 ;
                while($i < 100)
                {
                        $response = $this->postRequest($url, array("APIKEY: ". $this->apikey) );

                        if(isset($response['error'])){
                                return array("error" => "poll_fail");
                        }

                        if(in_array( $response['status'], array( "waiting", "processing") )){
                                if( $response["status"] == "failed"){
                                        return array("error" => "processing_failed");
                                }

                        }

                        if( $response["status"] == "archived"){
                                return array("error" => "file_archived");
                        }

                        if( $response["status"] == "finished"){
                                if( isset($response["links"]) && isset($response["links"]["download"]) ){
                                        return array("success" => $response["links"]["download"]);
                                }
                                else{
                                        return array("error" => "no_download_url");
                                }

                        }


                        sleep(3); //Wait 3 seconds
                        $i++;
                }
        }

        private function download($link,$filename){
                echo "Download result\n";
                $filename = str_replace(".txt",".pdf",$filename);

                //OS Call to download the file as pdf
                exec("curl $link --output $filename");
        }



        //Send HTTP Request with APIKEY as header
        private function postRequest($url,$headers, $content ="")
        {
                var_dump($url, $headers);
                $ch = curl_init();
                curl_setopt($ch, CURLOPT_URL, $url);
                curl_setopt($ch, CURLOPT_HTTPHEADER,$headers);
                curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
                if(!empty($content)){
                        curl_setopt($ch, CURLOPT_POST,           1 );
                        curl_setopt($ch, CURLOPT_POSTFIELDS,     $content );
                $output = curl_exec($ch);
                $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
                var_dump($http_code);
                var_dump($output);

                if($http_code[0] == 4){
                        $output = array( "error" => "invalid request");
                 }


                curl_close($ch);

                return json_decode($output,true);
				}
		}
}

?>
						