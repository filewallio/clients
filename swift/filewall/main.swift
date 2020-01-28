//
//  main.swift
//  filewall
//
//

import Foundation

func print_help()
{
    print("filewall -v <apikey> <source_file> <replace_source>")
    print("    -v            : verbose (optional)")
    print("    apikey        : your apikey")
    print("    source_file   : local file")
}

if(CommandLine.argc < 2) {
    print_help()
    exit(1)
}

let apikey = CommandLine.arguments[Int(CommandLine.argc-2)]
let source_file = CommandLine.arguments[Int(CommandLine.argc-1)]

if(!FileManager.default.fileExists(atPath: source_file)){
    print("File '\(source_file)' does not exist")
    exit(1)
}

let source_path = (source_file as NSString).deletingLastPathComponent
let source_name = (source_file as NSString).lastPathComponent
let source_content = try String(contentsOfFile: source_file)
let dest_path = (source_file as NSString).deletingPathExtension + ".pdf";

let filewall = Filewall(apikey: apikey)
filewall.convert(source_name, source_content){ dt, err in
    if(err != nil)
    {
        print("Error:", err!)
        exit(1)
    }
    
    try? dt?.write(to: URL(fileURLWithPath: dest_path))
    exit(0)
}

RunLoop.main.run()
