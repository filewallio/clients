//
//  Filewall.swift
//  filewall
//
//

import Foundation
import Alamofire

class Filewall {
    private var apikey: String?
    private var item_url: String?
    private var upload_url: String?
    private var download_url: String?
    
    init(apikey: String) {
        self.apikey = apikey
    }
    
    func convert(_ source_filename: String,_ source_content: String, _ completion: @escaping (_ data: Data?, _ error: String?) -> Void) {
        self.authorize { result in
            if(result != nil)
            {
                completion(nil, result)
                return
            }
            
            self.upload(source_filename, source_content) { result in
                if(!result)
                {
                    completion(nil, "upload_failed")
                    return
                }
                
                self.poll() { result in
                    if(result != nil)
                    {
                        completion(nil, result)
                        return
                    }
                    
                    self.download() { result in
                        if(result != nil)
                        {
                            completion(result, nil)
                        }
                        else
                        {
                            completion(nil, "download_failed")
                        }
                    }
                }
            }
        }
    }
    
    private func authorize(_ completion: @escaping (_ error: String?) -> Void)
    {
        AF.request("https://filewall.io/api/authorize", headers: ["APIKEY": self.apikey!]).responseJSON { response in
            switch response.result {
            case .success(let json):
                let res = json as! NSDictionary
                if let err = res["error"] as! String?
                {
                    completion(err)
                }
                else
                {
                    self.upload_url = (res["links"] as! NSDictionary)["upload"] as! String?
                    self.item_url = (res["links"] as! NSDictionary)["self"] as! String?
                    completion(nil)
                }
            case .failure:
                completion("unknown_error")
            }
        }
    }
    
    private func upload(_ file_name: String, _ content: String, _ completion: @escaping (_ result: Bool) -> Void)
    {
        AF.upload(Data(content.utf8), to: self.upload_url!, headers: ["filename": file_name]).responseJSON { response in
            switch response.result {
            case .success:
                completion(true)
            case .failure:
                completion(false)
            }
        }
    }
    
    private func poll(_ completion: @escaping (_ result: String?) -> Void)
    {
        AF.request(self.item_url!, headers: ["APIKEY": self.apikey!]).responseJSON { response in
            switch response.result {
            case .success(let json):
                let res = json as! NSDictionary
                
                if let err = res["error"] as! String?
                {
                    completion(err)
                    return
                }
                
                if let status = res["status"] as! String?
                {
                    if(status == "failed")
                    {
                        completion("processing_failed")
                        return
                    }
                    if(status == "archived")
                    {
                        completion("file_archived")
                        return
                    }
                    if(status == "finished")
                    {
                        if let dl_url = (res["links"] as! NSDictionary)["download"] as! String?
                        {
                            self.download_url = dl_url
                            completion(nil)
                        }
                        else
                        {
                            completion("no_download_url")
                        }
                        return
                    }
                }
            case .failure:
                completion("processing_failed")
                return
            }
            sleep(3)
            self.poll() { result in
                completion(result)
            }
        }
    }
    
    private func download(_ completion: @escaping (_ result: Data?) -> Void)
    {
        AF.download(self.download_url!).responseData { response in
            if let dl_data = response.value {
                completion(dl_data)
            }
            else
            {
                completion(nil)
            }
        }
    }
}
