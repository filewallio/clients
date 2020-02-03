-module(erl_filewall).

-export([convert/3, convert/4]).

-define(API_TIMEOUT, 5000).

convert(ApiKey, Filename, Content) ->
    convert(ApiKey, Filename, Content, false).

convert(ApiKey, Filename, Content, Verbose) when 
      is_list(ApiKey), is_list(Filename), (is_list(Content) orelse is_binary(Content)),
      (Verbose =:= true orelse Verbose =:= false) ->

    try handle(ApiKey, Filename, Content, Verbose) of
        {ok, _D} = R ->
            R
    catch
        error:{badmatch, Tuple} ->
            Tuple;
        Excp:Reason ->
            Stk = erlang:get_stacktrace(),
            {error, {Excp, Reason, Stk}}
    end.

handle(ApiKey, Filename, Content, Verbose) ->
    {ok, Links} = authorize(ApiKey),
    {<<"upload">>, UploadLink} = lists:keyfind(<<"upload">>, 1, Links),
    maybe_log("authorize OK: upload=~s~n", [UploadLink], Verbose),

    ok = upload(binary_to_list(UploadLink), Filename, Content),
    {<<"self">>, PollLink} = lists:keyfind(<<"self">>, 1, Links),
    maybe_log("upload OK: poll=~s~n", [PollLink], Verbose),

    {ok, DownloadURL} = poll(binary_to_list(PollLink), ApiKey, Verbose),
    maybe_log("poll OK: url=~s~n", [DownloadURL], Verbose),

    {ok, {Filename2, Content2}} = download(binary_to_list(DownloadURL)),
    maybe_log("download OK: filename=~s len=~p~n", [Filename2, length(Content2)], Verbose),

    {ok, {Filename2, Content2}}.

download(Url) when is_list(Url) ->
    Req = {Url, []},
    Res = httpc:request(get, Req, [{timeout, ?API_TIMEOUT}], []),
    case Res of
        {ok, {{_, 200, _}, Headers, Body}} ->
            {"content-disposition", H} = lists:keyfind("content-disposition", 1, Headers),
            {ok, Filename} = parse_filename(H),
            {ok, {Filename, Body}};
        {ok, {{_, _Code, _}, _, Body}} ->
            {struct, L} = mochijson2:decode(Body),
            {<<"error">>, Err} = lists:keyfind(<<"error">>, 1, L),
            {error, Err};
        {error, Reason} -> 
            {error, Reason}
    end.

parse_filename(H) when H =:= false; H =:= "" ->
    {ok, ""};
parse_filename(H) ->
    Tokens = string:tokens(H, "="),
    case length(Tokens) =:= 2 of
        true ->
            [_Head|[T]] = Tokens,
            {ok, string:strip(T, both, $")};
        false ->
            {ok, ""}
    end.

poll(PollLink, ApiKey, Verbose) when is_list(PollLink) ->
    do_poll(PollLink, ApiKey, Verbose, 1).

do_poll(_PollLink, _ApiKey, _Verbose, 100) ->
    {error, <<"poll timeout exceeded">>};
do_poll(PollLink, ApiKey, Verbose, Counter) ->
    Req = {PollLink, [{"APIKEY", ApiKey}], "", ""},
    maybe_log("poll #~p~n", [Counter], Verbose),
    Res = httpc:request(post, Req, [{timeout, ?API_TIMEOUT}], []),
    case Res of
        {ok, {{_, 200, _}, _, Body}} ->
            {struct, L} = mochijson2:decode(Body),
            {<<"status">>, Status} = lists:keyfind(<<"status">>, 1, L),
            case handle_status(Status) of
                {ok, done} ->
                    process_finished(L);
                {ok, more} ->
                    do_poll(PollLink, ApiKey, Verbose, Counter + 1);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, {{_, _Code, _}, _, Body}} ->
            {struct, L} = mochijson2:decode(Body),
            {<<"error">>, Err} = lists:keyfind(<<"error">>, 1, L),
            {error, Err};
        {error, Reason} -> 
            {error, Reason}
    end.

handle_status(false) -> 
    timer:sleep(3000),
    {ok, more};
handle_status(<<"waiting">>) ->
    timer:sleep(3000),
    {ok, more};
handle_status(<<"processing">>) ->
    timer:sleep(3000),
    {ok, more};
handle_status(<<"failed">>) ->
    {error, <<"processing_failed">>};
handle_status(<<"archived">>) ->
    {error, <<"file_archived">>};
handle_status(<<"finished">>) ->
    {ok, done};
handle_status(_Any) ->
    timer:sleep(3000),
    {ok, more}.

process_finished(L) ->
    {<<"links">>, Links} = lists:keyfind(<<"links">>, 1, L),
    case Links of
        false ->
            {error, <<"no_download_url">>};
        {struct, []} ->
            {error, <<"no_download_url">>};
        {struct, Links2} ->
            {<<"download">>, Url} = lists:keyfind(<<"download">>, 1, Links2),
            case Url of
                false ->
                    {error, <<"no_download_url">>};
                Url ->
                    {ok, Url}
            end
    end.

upload(UploadLink, Filename, Content) when is_list(UploadLink) ->
    Req = {UploadLink, [{"filename", Filename}], 
            "application/x-www-form-urlencoded", Content},
    Res = httpc:request(post, Req, [{timeout, ?API_TIMEOUT}], []),
    case Res of
        {ok, {{_, 202, _}, _, _Body}} ->
            ok;
        {ok, {{_, _Code, _}, _, _Body}} ->
            {error, <<"one of the request parameters are not send correctly">>};
        {error, Reason} -> 
            {error, Reason}
    end.

authorize(ApiKey) ->
    Req = {"https://filewall.io/api/authorize", [{"APIKEY", ApiKey}], "", ""},
    Res = httpc:request(post, Req, [{timeout, ?API_TIMEOUT}], []),
    case Res of
        {ok, {{_, 202, _}, _, Body}} ->
            {struct, L} = mochijson2:decode(Body),
            {<<"links">>, {struct, Links}} = lists:keyfind(<<"links">>, 1, L),
            {ok, Links};
        {ok, {{_, _Code, _}, _, Body}} ->
            {struct, L} = mochijson2:decode(Body),
            {<<"error">>, Err} = lists:keyfind(<<"error">>, 1, L),
            {error, Err};
        {error, Reason} -> 
            {error, Reason}
    end.

maybe_log(Format, Data, true) ->
    io:format(Format, Data);
maybe_log(_Format, _Data, false) -> ok.

