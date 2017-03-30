-module(groklib).

-export([build_pattern/2, match/3, get_subpatterns/1]).

-type exp_pattern() :: {Metadata :: [string()], CompiledRegExp :: re:mp()}.
-export_type([exp_pattern/0]).

-define(BACKSLASH, $\\).

%%==================================================================== 
%% API functions

%%--------------------------------------------------------------------
%% Returns metadata of the pattern and resulting compiled regular expression
%%
-spec build_pattern(AppPattern :: list(), CorePatterns :: map()) -> tuple().

build_pattern(AppPattern, CorePatterns) ->
    Metadata = extract_metadata(AppPattern),
    RegExp = expand_pattern(AppPattern, CorePatterns),
    CompiledRegExp = compile_pattern(RegExp),
    {Metadata, CompiledRegExp}.

%%--------------------------------------------------------------------
%% Receives text to match, metadata and regular expression.
%% Returns either nomatch or captured data
%%
-spec match(Text :: string(), Metadata :: list(), RE :: string()) -> nomatch | map().

match(Text, Metadata, RegExp) ->
    case re:run(Text, RegExp, [global, {capture, all_but_first, list}]) of
        {match, [Captured|_]} ->
            convert_types(Captured, Metadata);
        nomatch ->
            nomatch
    end.

%%--------------------------------------------------------------------
%% Receives pattern
%% Returns names of included grok subpatterns
%%
-spec get_subpatterns(Pattern :: string()) -> [string].

get_subpatterns(Pattern) ->
    [X || [_, X |_] <- extract_names(Pattern)].

%%====================================================================
%% Private functions

%%====================================================================
%% Utility functions for pattern expansion and compilation

%%--------------------------------------------------------------------
expand_pattern(Pattern, Patterns) ->
    %io:format("***** Entering high level expansion with ~p~n", [Pattern]),
    Pattern1 = expand_high_level(Pattern, Patterns),

    %io:format("***** Entering low level expansion with: ~p~n", [Pattern1]),
    Pattern2 = expand_low_level(Pattern1, Patterns),

    case re:run(Pattern2, "%{\\w+(:\\w+)?}", [ungreedy]) of 
        nomatch ->
            Pattern2;
        {match, _} ->
            expand_pattern(Pattern2, Patterns)
    end.

%%--------------------------------------------------------------------
expand_high_level(Pattern, Patterns) ->
    case re:run(Pattern, "%{(\\w+):(\\w+)(?::\\w+)?}", [ungreedy, {capture, all, list}]) of
        nomatch -> 
            Pattern;
        {match, [String, Type, Name|_]} ->
            Replacement = maps:get(Type, Patterns),
            Replacement1 = escape("(?P<" ++ Name ++ ">" ++ Replacement ++ ")"),
            %io:format("~p -> ~p~n", [Type, Replacement1]),
            NewPattern = re:replace(Pattern, String, Replacement1, [ungreedy, {return, list}]),
            %io:format("~p~n", [NewPattern]),
            expand_high_level(NewPattern, Patterns)
    end.

%%--------------------------------------------------------------------
expand_low_level(Pattern, Patterns) ->
     case re:run(Pattern, "%{(\\w+)}", [ungreedy, {capture, all, list}]) of
        nomatch -> 
            Pattern;
        {match, [String, Type|_]} ->
            Replacement = maps:get(Type, Patterns),
            Replacement1 = escape(Replacement),
            %io:format("~p -> ~p~n", [Type, Replacement1]),
            NewPattern = re:replace(Pattern, String, Replacement1, [ungreedy, {return, list}]),
            %io:format("~p~n", [NewPattern]),
            expand_low_level(NewPattern, Patterns)
    end.

%%--------------------------------------------------------------------
compile_pattern(P) ->
    {ok, MP} = re:compile(P),
    MP.

%%--------------------------------------------------------------------
escape(Str) ->
    escape(Str, []).

escape([], Rslt) ->
    lists:reverse(Rslt);

escape([H|T], Rslt) ->
    case H =:= ?BACKSLASH of
        true ->
            escape(T, [H | [H | Rslt]]);
        false ->
            escape(T, [H | Rslt])
    end.

%%====================================================================
%% Utility functions for meatadata extraction

extract_metadata(Pattern) ->
   Names = extract_names(Pattern),
   Defaults = set_defaults(Names),
   Types = extract_types(Pattern),
   merge_names_types(Defaults, Types).

%%--------------------------------------------------------------------
extract_names(Pattern) ->
    {match, Captured} = re:run(Pattern, "%{(\\w+):(\\w+)(?::\\w+)?}", [ungreedy, global, {capture,all_but_first,list}]),
    Captured.

%%--------------------------------------------------------------------
set_defaults(Names) ->
    %lists:map(fun([_V, K | _]) -> {list_to_atom(K), undefined} end, Names).
    [{list_to_atom(X), undefined} || [_, X |_] <- Names].

%%--------------------------------------------------------------------
extract_types(Pattern) ->
    case re:run(Pattern, "%{(\\w+):(\\w+):(\\w+)}", [ungreedy, global, {capture,all_but_first,list}]) of
        {match, Captured} ->
            lists:map(fun([_V, K, T | _]) -> {list_to_atom(K), list_to_atom(T)} end, Captured);
        nomatch ->
            []
    end.

%%--------------------------------------------------------------------
merge_names_types(Names, Types) ->
    merge_names_types(Names, Types, []).

merge_names_types([], _, Merged) ->
    lists:reverse(Merged);

merge_names_types([{Name, Type}|Names], Types, Merged) ->
    T = case get_type(Name, Types) of 
            undefined ->
                Type;
            Tp ->
                Tp
        end,
    merge_names_types(Names, Types, [{Name, T}|Merged]).

%%--------------------------------------------------------------------
get_type(_, []) ->
    undefined;

get_type(Name, [{Name, Type}|_]) ->
    Type;

get_type(Name, [_|Types]) ->
    get_type(Name, Types).

%%====================================================================
%% Utility functions for type conversion

%%--------------------------------------------------------------------
convert_types(Data, Metadata) ->
    convert_types(Data, Metadata, #{}).

convert_types([], [], Result) ->
    Result;

convert_types([Value|Data], [{Name, Type}|Metadata], Result) ->
   convert_types(Data, Metadata, maps:put(atom_to_list(list_to_binary(Name)), convert_type(Type, Value), Result)).

%%--------------------------------------------------------------------
convert_type(int, Val) ->
    list_to_integer(Val);

convert_type(float, Val) ->
    list_to_float(Val);

convert_type(list, Val) ->
    Val;

convert_type(erlang_timestamp, Val) ->
    UnixTS = list_to_integer(Val),
    {UnixTS div 1000000, UnixTS rem 1000000, 0};

convert_type(_, Val) ->
    list_to_binary(Val).

