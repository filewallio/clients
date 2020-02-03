### Overview

Lua-script show how to interact with filewall api.
Inside 2 files:

**filewall.lua** -- filewall library
**test.lua** -- cli example

### Deps
Installing lua and luarocks(lua packet-manager)

```sh
sudo apt install lua5.3 luarocks
```

Installing addional lua libraries

```sh
sudo luarocks install luasocket
sudo luarocks install lua-cjson 
sudo luarocks install luasec
sudo luarocks install lua-path
```

### Use

```sh
test.lua -v <apikey> <source_file>
    -v            : verbose (optional)
    apikey        : your apikey
    source_file   : local file


lua ./test.lua -v 577123ae-4821-4bc8-a8c2-a510b96f47d8 ./1.txt
```


