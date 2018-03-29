-module(groklib).

-export([build_pattern/2,
         match/3, 
         get_subpatterns/1, 
         get_pattern_metadata/1, 
         expand_pattern/2,
         escape/1,
         unescape/1]).

-type grok_metadata() :: [{string(), atom()}].
-type exp_pattern() :: {grok_metadata(), CompiledRegExp :: re:mp()}.
-export_type([exp_pattern/0, grok_metadata/0]).

-define(BACKSLASH, $\\).

%%==================================================================== 
%% API functions

%%--------------------------------------------------------------------
%% Returns metadata of the pattern and resulting compiled regular expression
%%
-spec build_pattern(AppPattern :: [string()], CorePatterns :: #{Name :: string() => Pattern :: string()}) -> exp_pattern().
build_pattern(AppPattern, CorePatterns) ->
    Metadata = extract_metadata(AppPattern),
    RegExp = expand_pattern(AppPattern, CorePatterns),
    CompiledRegExp = compile_pattern(RegExp),
    {Metadata, CompiledRegExp}.

%%--------------------------------------------------------------------
%% Receives text to match, metadata and regular expression.
%% Returns either nomatch or captured data
%%
-spec match(Text :: string(), Metadata :: list(), RE :: string()) -> nomatch | #{Name :: string => Value :: term()}.
match(Text, Metadata, RegExp) ->
    case re:run(unicode:characters_to_binary(Text), RegExp, [global, {capture, all_but_first, binary}]) of
        {match, [Captured|_]} ->
            convert_types(Captured, Metadata);
        nomatch ->
            nomatch
    end.

%%--------------------------------------------------------------------
%% Receives pattern
%% Returns names of included grok subpatterns
%%
-spec get_subpatterns(Pattern :: string()) -> [string()].
get_subpatterns(Pattern) ->
    [X || [_, X |_] <- extract_names(Pattern)].

%%--------------------------------------------------------------------
%% Receives pattern
%% Returns complete metadata of the pattern
%%
-spec get_pattern_metadata(Pattern :: string()) -> grok_metadata().
get_pattern_metadata(Pattern) ->
    extract_metadata(Pattern).

%%--------------------------------------------------------------------
%% Expands pattern with Patterns into returned regular expression
%%
-spec expand_pattern(Pattern :: string(), Patterns :: [string()]) -> string().
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
%% Doubles backslash characters
%%
-spec escape(Str :: string()) -> string().
escape(Str) ->
    lists:flatten(string:replace(Str, "\\", "\\\\", all)).

%%--------------------------------------------------------------------
%% Replaces double backslash charackers with single ones.
%%
-spec unescape(Str :: string()) -> string().
unescape(Str) ->
    lists:flatten(string:replace(Str, "\\\\", "\\", all)).

%%====================================================================
%% Private functions

%%====================================================================
%% Utility functions for pattern expansion and compilation

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
    {ok, MP} = re:compile(P, [unicode]),
    MP.

%%====================================================================
%% Utility functions for meatadata extraction
%%
extract_metadata(Pattern) ->
   Names = extract_names(Pattern),
   Defaults = set_defaults(Names),
   Types = extract_types(Pattern),
   merge_names_types(Defaults, Types).

%%--------------------------------------------------------------------
extract_names(Pattern) ->
    case re:run(Pattern, "%{(\\w+):(\\w+)(?::\\w+)?}", [ungreedy, global, {capture,all_but_first,list}]) of
        {match, Captured} ->
            Captured;
        nomatch ->
            []
    end.

%%--------------------------------------------------------------------
set_defaults(Names) ->
    %lists:map(fun([_V, K | _]) -> {list_to_atom(K), undefined} end, Names).
    [{X, undefined} || [_, X |_] <- Names].

%%--------------------------------------------------------------------
extract_types(Pattern) ->
    case re:run(Pattern, "%{(\\w+):(\\w+):(\\w+)}", [ungreedy, global, {capture,all_but_first,list}]) of
        {match, Captured} ->
            lists:map(fun([_V, K, T | _]) -> {K, list_to_atom(T)} end, Captured);
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
   convert_types(Data, Metadata, maps:put(Name, convert_type(Type, Value), Result)).

%%--------------------------------------------------------------------
%% match captures binaries so input value Val is always a binary.
%% We prefer binary because our groklib clients require binaries in
%% most cases. Although conversion from list to other types is 
%% easier than conversion from binary.
%%
convert_type(binary, Val) ->
    Val;

convert_type(int, Val) ->
    list_to_integer(binary_to_list(Val));

convert_type(float, Val) ->
    list_to_float(binary_to_list(Val));

convert_type(list, Val) ->
    unicode:characters_to_list(Val);

convert_type(_, Val) ->
    Val.
