local socket = require("socket")
local ltn12 = require("ltn12")
local cjson = require "cjson"
local https = require("ssl.https")

local _M = { _VERSION = '0.01' }

local mt = { __index = _M }

local function log(self, line)
    if self.verbose == true then
        print(line)
    end
end

local function authorize(api_key) 
    local response_body = {}
    local res, code, response_headers, status = https.request{
        url = "https://filewall.io/api/authorize",
        method = "POST", 
        headers = {
            ["APIKEY"] = api_key
        },
        protocol = "tlsv1_2",
        sink = ltn12.sink.table(response_body),
    }
    response_body = table.concat(response_body or {})

    local json = cjson.decode(response_body)
    if code ~= 202 then
        return nil, json["error"] or response_body
    end

    return json
end

local function upload(upload_url, filename, content)
    local response_body = {}
    local res, code, response_headers, status = https.request{
        url = upload_url,
        method = "POST", 
        headers = {
            ["Content-Type"] = "application/x-www-form-urlencoded";
            ["Content-Length"] = #content;
            ["filename"] = filename;
          },
           protocol = "tlsv1_2",
        source = ltn12.source.string(content),
        sink = ltn12.sink.table(response_body),
    }
    response_body = table.concat(response_body or {})
    if code ~= 202 then
        return nil, "one of the request parameters are not send correctly"
    end

    return true
end

local function poll(item_url, api_key, self)
    for i=1,100 do
        log(self, "poll #" .. tostring(i))
        local response_body = {}
        local res, code, response_headers, status = https.request{
            url = item_url,
            method = "POST", 
            headers = {
                ["APIKEY"] = api_key
            },
            protocol = "tlsv1_2",
            sink = ltn12.sink.table(response_body),
        }
        response_body = table.concat(response_body or {})

        local json = cjson.decode(response_body)

        if code ~= 200 then
            return nil, json["error"] or response_body
        end
        
        if json["status"] then
            local status = json["status"]

            if status ~= "waiting" and status ~= "processing" then
                if status == "failed" then
                    return nil, "processing_failed"
                end

                if status == "archived" then
                    return nil, "file_archived"
                end

                if status == "finished" then
                    if json["links"] and json["links"]["download"] then
                        return true, json["links"]["download"]
                    else
                        return nil, "no_download_url"
                    end
                end
            end
        end

        socket.sleep(3)
    end

    return nil, "poll timeout exceeded"
end

local function download(download_url)
    local response_body = {}
    local res, code, response_headers, status = https.request{
        url = download_url,
        method = "GET", 
        protocol = "tlsv1_2",
        sink = ltn12.sink.table(response_body),
    }
    response_body = table.concat(response_body or {})

    if code ~= 200 then
        local json = cjson.decode(response_body)
        return nil, json["error"] or response_body
    end

    local filename = string.match(response_headers["content-disposition"] or "", 'filename="(.*)"')

    return filename or "", response_body
end

function _M.new(self, api_key, verbose)
    return setmetatable({api_key = api_key or "", verbose = verbose or false}, mt)
end

function _M.convert(self, source_filename, source_content)
    local api_key = self.api_key
    local res, err = authorize(api_key)
    if not res then
        return nil, err
    end
    log(self, "authorize OK: upload=" .. res["links"]["upload"])

    local ok, err = upload(res["links"]["upload"], source_filename, source_content)
    if not ok then
        return nil, err
    end
    log(self, "upload OK: poll=" .. res["links"]["self"])
    
    local ok, url = poll(res["links"]["self"], api_key, self)
    if not ok then
        return nil, url
    end
    log(self, "poll OK: url=" .. url)

    local filename, data = download(url)
    if not filename then
        return nil, data
    end
    log(self, "download OK: filename=".. filename .. " len=" .. string.len(data))

    return filename, data
end

return _M
