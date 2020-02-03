use std::fs::File;
use std::thread;
use std::io::Read;

use serde_json::Value;
use reqwest::{Error, blocking::Client, blocking::Response};

use log::{info};

pub struct FileWall {
    apikey: String,
}

impl FileWall {
    pub fn new(apikey: &str) -> Self {
        FileWall { apikey: String::from(apikey) }
    }

    pub fn convert(&self, source_filename: &str, source_file: File)
                   -> Result<(String, Vec<u8>), Error>{
        let raw_response = self.authorize().unwrap();
        let response: Value = serde_json::from_str(&raw_response.text().unwrap()).unwrap();
        let url = response["links"]["upload"].as_str().unwrap();

        self.upload(url, source_filename, source_file)?;
        let download_url = self.poll(response["links"]["self"].as_str().unwrap());

        return self.download(&download_url.unwrap());
    }

    fn authorize(&self) -> Result<Response, Error> {
        info!("Authorize");
        let client = Client::new();

        let response = client.post("https://filewall.io/api/authorize")
                             .header("APIKEY", &self.apikey)
                             .send()?;

        if response.status() == 202 {
            return Ok(response);
        }

        Ok(FileWall::print_error(response))
    }

    fn upload(&self, upload_url: &str,
              filename: &str, file: File)
             -> Result<Response, Error>{

        info!("Upload");
        let client = Client::new();

        let response = client.post(upload_url)
                             .body(file)
                             .header("filename", filename)
                             .send()?;

        if response.status() == 202 {
            return Ok(response);
        }

        Ok(FileWall::print_error(response))
    }

    fn poll(&self, item_url: &str) -> Result<String, Error> {
        for _ in 0..100 {
            info!("Waiting for result");

            let client = Client::new();
            let response = client.get(item_url)
                              .header("APIKEY", &self.apikey)
                              .send()?;
            let response: Value = serde_json::from_str(&response.text()?).unwrap();

            if response["error"].is_object() {
                panic!("Error: {}", response["error"].as_str().unwrap());
            }

            if response.get("status").is_some() {
                let status = response["status"].as_str().unwrap();
                if status != "waiting" && status != "processing" {
                    if status == "failed" {
                        panic!("Failed: Failed");
                    }
                    if status == "archived" {
                        panic!("Failed: Archived");
                    }
                    if status == "finished" {
                        if response["links"].is_object() && response["links"].get("download").is_some() {
                            return Ok(String::from(response["links"]["download"].as_str().unwrap()));
                        }
                        else {
                            panic!("Failed: No Download URL");
                        }
                    }
                }
            }

            thread::sleep(std::time::Duration::new(3, 0));
        }
        panic!("Couldn't get the file!")
    }

    fn download(&self, download_url: &str) -> Result<(String, Vec<u8>), Error>{
        info!("Download results");

        let mut response = reqwest::blocking::get(download_url).unwrap();
        let mut data = Vec::new();
        // write in bytes to data
        response.read_to_end(&mut data).unwrap();

        let tmp = response.headers()
                          .get("content-disposition")
                          .unwrap()
                          .to_str()
                          .unwrap()
                          .split("; filename=\"")
                          .collect::<Vec<&str>>();

        let tmp = tmp.last()
                     .unwrap()
                     .split("\"")
                     .collect::<Vec<&str>>();

        let filename = tmp.first()
                          .unwrap();

        return Ok((String::from(filename.clone()), data))
    }

    fn print_error(response: Response) -> Response {
        let json: Value = serde_json::from_str(&response.text().unwrap()).unwrap();
        panic!("error: {}", json["error"].as_str().unwrap())
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use super::FileWall;

    #[test]
    fn basics() {
        let obj = FileWall::new("you apikey");
        let foo = File::open("foo.txt").unwrap();
        obj.convert("foo.txt", foo).unwrap();
    }
}
