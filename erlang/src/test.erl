-module(test).

-export([test/2]).

test(ApiKey, File) when is_list(ApiKey), is_list(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            convert(ApiKey, File, Bin);
        {error, Reason} ->
            io:format("open file=~p failed: ~p~n", [File, Reason]),
            {error, Reason}
    end.

convert(ApiKey, File, Bin) ->
    SourceName = filename:basename(File),
    SourcePath = filename:dirname(filename:absname(File)),

    case erl_filewall:convert(ApiKey, SourceName, Bin, true) of
        {ok, {Fname, Content}} ->
            handle_save(SourcePath, Fname, Content);
        {error, Reason} ->
            io:format("filewall convert failed: ~p~n", [Reason]),
            {error, Reason}
     end.

handle_save(SourcePath, Fname, Content) ->
    handle_save(SourcePath, Fname, Fname, Content, 1).

handle_save(SourcePath, Fname, Fname2, Content, C) ->
    OutputFile = filename:join([SourcePath, Fname2]),
    case filelib:is_regular(OutputFile) of
        true ->
            Ext = filename:extension(Fname),
            Fname3 = filename:basename(Fname, Ext),
            OutputFile2 = Fname3 ++ "_" ++ integer_to_list(C) ++ Ext,
            handle_save(SourcePath, Fname, OutputFile2, Content, C + 1);
        false ->
            file:write_file(OutputFile, Content),
            ok
    end.

