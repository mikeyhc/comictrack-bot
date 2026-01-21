-module(comic_volume).

-export([start_year_sort/2, name_sort/2]).
-export([full_name/1, full_name/2]).
-export([name_fuzzymatcher/1]).

-define(MATCH_CUTOFF, 0.7).

start_year_sort(#{<<"start_year">> := A}, #{<<"start_year">> := B}) ->
    A > B.

name_sort(#{<<"name">> := A}, #{<<"name">> := B}) ->
    A < B.

full_name(Volume) -> full_name(Volume, #{}).

full_name(#{<<"name">> := Name,
            <<"count_of_issues">> := IssueCount,
            <<"start_year">> := StartYear},
          Options) ->
    SafeName = case maps:get(name_length, Options, undefined) of
                   undefined -> Name;
                   Length ->
                       if size(Name) > Length ->
                              Head = binary:part(Name, 0, Length - 3),
                              <<Head/binary, "...">>;
                          true -> Name
                       end
               end,
    StartYearBin = if StartYear =:= null -> <<"">>;
                      true -> <<" [", StartYear/binary, "] ">>
                   end,
    BinCount = integer_to_binary(IssueCount),
    <<SafeName/binary, StartYearBin/binary, "(", BinCount/binary, " issues)">>.

build_fuzzymatcher(Target) ->
    SearchName = string:casefold(Target),
    fun(Str) ->
        S = string:casefold(Str),
        case string:find(S, SearchName) of
            nomatch -> string:jaro_similarity(S, SearchName) > ?MATCH_CUTOFF;
            _Match -> true
        end
    end.

name_fuzzymatcher(Name) ->
    Matcher = build_fuzzymatcher(Name),
    fun(#{<<"name">> := N}) -> Matcher(N) end.
