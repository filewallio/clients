local filewall = require "filewall"
local path = require "path"

if arg[1] == "help" or #arg < 2 then
    print("test.lua -v <apikey> <source_file>")
    print("    -v            : verbose (optional)")
    print("    apikey        : your apikey")
    print("    source_file   : local file")
    return
end

local source_file = arg[#arg]
local api_key = arg[#arg - 1]
local verbose = false

for k, v in pairs(arg) do
    if v == "-v" then
        verbose = true
        break
    end
end

local source_path = path.dirname(path.abspath(source_file))
local source_name = path.basename(source_file)

local f, err = io.open(source_file, "rb")
if not f then
    print("open file=.."..source_file.." failed: " .. err)
    return
end
local source_content = f:read("*all")
f:close()

--local api_key = '577123ae-4821-4bc8-a8c2-a510b96f47d8'
local fw = filewall:new(api_key, verbose)
local fname, data = fw:convert(source_name, source_content)
if not fname then
    print("filewall convert failed: " .. data)
    return
end

local i = 1
local output_file = path.join(source_path, fname)
while true do
    if not path.exists(output_file) then
        local f, err = io.open(output_file, "wb")
        if not f then
            print("open file=.."..output_file.." failed: " .. err)
            return
        end
        f:write(data)
        f:close()
        break
    end

    local fn, ext = path.splitext(fname)
    local new_filename = fn .. "_" .. tostring(i) .. ext
    output_file = path.join(source_path, new_filename)
    i = i + 1
end
