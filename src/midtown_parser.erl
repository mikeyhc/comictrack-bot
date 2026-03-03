-module(midtown_parser).

-export([parse/1]).

parse(HtmlBin) ->
    {htmlDocument, _, _, _, [Document]}= htmerl:simple(HtmlBin),
    Matches = recursive_find(<<"product-side">>, Document),
    Issues = generate_issues_from_html(Matches),
    Volume0 = generate_volume_block(Issues),
    VolumeName = maps:get(<<"name">>, Volume0),
    {ok, VolumeId} = id_generator:get_volume_id(VolumeName),
    VolumeBlock = #{<<"id">> => VolumeId,
                    <<"name">> => VolumeName
                   },
    WithVolume = lists:map(fun(I) -> I#{<<"volume">> => VolumeBlock} end,
                           Issues),
    UniqVolumes = lists:uniq(fun(#{<<"issue_number">> := N}) -> N end,
                             WithVolume),
    Sorted = lists:sort(fun comic_issue:issue_number_sort/2, UniqVolumes),
    WithIds = lists:map(fun(I=#{<<"issue_number">> := IssueNumber}) ->
                                {ok, Id} = id_generator:get_issue_id(
                                             VolumeId,
                                             IssueNumber),
                                I#{<<"id">> => Id}
                        end, Sorted),
    Volume1 = Volume0#{<<"id">> => VolumeId,
                       <<"count_of_issues">> => length(Sorted),
                       <<"issues">> => lists:map(fun issue_stub/1, WithIds)
                      },
    {Volume1, WithIds}.

%% helper methods

recursive_find(Key, Elem={htmlElement, _, _, Attributes, Children}) ->
    case class_match(Key, Attributes) of
        true -> Elem;
        false ->
            lists:flatten(
              lists:map(fun(Child) -> recursive_find(Key, Child) end, Children))
    end;
recursive_find(_, _) -> [].

class_match(Key, Attributes) ->
    lists:any(fun({_, <<"class">>, _, _, Values}) ->
                      Parts = string:split(Values, <<" ">>, all),
                      lists:any(fun(P) -> P =:= Key end, Parts);
                 (_) -> false
              end, Attributes).

generate_issues_from_html(Html) ->
    Unstandard = lists:map(fun generate_issue_from_html/1, Html),
    standardize_issues(Unstandard).

generate_issue_from_html({_, _, _, _, Children}) ->
    [_,
     {_, _, _, _, [_, TitleAnchor]},
     _,
     {_, _, _, _, [_, PublisherDl,_]}|_] = Children,
    {_, _, _, _, PublisherEntries} = PublisherDl,
    {VolumeName, IssueNumber} = parse_issue_name(TitleAnchor),
    PublisherValues = lists:filter(fun({_, <<$d, _/binary>>, _, _, _}) -> true;
                                      (_) -> false
                                   end, PublisherEntries),
    {Dts, Dds} = lists:partition(fun({_, <<_:8, $t>>, _, _, _}) -> true;
                                    (_) -> false
                                 end, PublisherValues),
    AddPublisherEntry = fun({{_, _, _, _, [Dt]},
                             {_, _, _, _, Dd}},
                             Acc) ->
                                Acc#{clean_dt(Dt) => extract_dd_value(Dd)}
                        end,
    lists:foldl(AddPublisherEntry,
                #{<<"volume">> => VolumeName,
                  <<"issue_number">> => IssueNumber},
                lists:zip(Dts, Dds)).


parse_issue_name(Anchor) ->
    {_, _, _, _, [TitleHtml]} = Anchor,
    {_, _, _, _, [{htmlText, Text, text}]} = TitleHtml,
    Parts = string:split(Text, <<" ">>, all),
    NotNumber = fun(<<$#, _/binary>>) -> false; (_) -> true end,
    {VolumeParts, [FullIssueBin|_]} = lists:splitwith(NotNumber, Parts),
    <<$#, IssueNumber/binary>> = FullIssueBin,
    {binary_join(VolumeParts, <<" ">>), IssueNumber}.

extract_dd_value([Element]) ->
    extract_dd_value_(Element);
extract_dd_value([_, Element]) ->
    extract_dd_value_(Element);
extract_dd_value([_, Element, _]) ->
    extract_dd_value_(Element).

extract_dd_value_({htmlText, Text, text}) -> Text;
extract_dd_value_({htmlElement, <<"span">>, _, _, [Text]}) ->
    extract_dd_value_(Text);
extract_dd_value_({htmlElement, <<"span">>, _, _, [_, Anchor, _]}) ->
    extract_dd_value_(Anchor);
extract_dd_value_({htmlElement, <<"a">>, _, _, [_, Span, _]}) ->
    extract_dd_value_(Span);
extract_dd_value_({htmlElement, <<"a">>, _, _, [Text]}) ->
    extract_dd_value_(Text).

clean_dt({htmlText, Title, text}) ->
    lists:foldl(fun(Fn, Acc) -> Fn(Acc) end,
                Title,
                [fun string:trim/1,
                 fun remove_parens/1,
                 fun remove_colons/1,
                 fun space_to_underscore/1,
                 fun string:lowercase/1
                ]).

remove_colons(Str) ->
    case binary:last(Str) of
        $: ->
            L = binary_to_list(Str),
            list_to_binary(lists:sublist(L, length(L) - 2));
        _ -> Str
    end.

remove_parens(V) ->
    binary:replace(V, <<"(s)">>, <<"s">>, [global]).

space_to_underscore(V) ->
    binary:replace(V, <<" ">>, <<"_">>, [global]).

standardize_issues(Issues) ->
    lists:map(fun standardize_issue/1, Issues).

standardize_issue(Issue) ->
    maps:from_list(
      lists:foldl(fun(Entry, Acc) ->
                          standardize_issue_entry(Entry) ++ Acc
                  end,
                  [{'_last_updated', erlang:system_time(seconds)},
                   {<<"name">>, null}],
                  maps:to_list(Issue))).

standardize_issue_entry({<<"release_date">>, Date}) ->
    [Month, Day, StarYear] = binary:split(Date, <<"/">>, [global]),
    Year = case StarYear of
               <<A:8, B:8, C:8, D:8, $*>> -> <<A, B, C, D>>;
               _ -> StarYear
           end,
    [{<<"store_date">>, <<Year/binary, "-", Month/binary, "-", Day/binary>>}];
standardize_issue_entry({<<"by">>, Publisher}) ->
    [{<<"publisher">>, Publisher}];
standardize_issue_entry(Entry) -> [Entry].

generate_volume_block(Issues) ->
    Counts = lists:foldl(fun count_volumes/2, #{}, Issues),
    [{VolumeName, _}|_] = lists:reverse(lists:keysort(2, maps:to_list(Counts))),
    StartYear = find_start_year(Issues),
    #{<<"name">> => VolumeName, <<"start_year">> => StartYear}.

count_volumes(#{<<"volume">> := VolumeName}, Counts) ->
    Current = maps:get(VolumeName, Counts, 0),
    Counts#{VolumeName => Current + 1}.

find_start_year([First|Rest]) ->
    GetYear = fun(#{<<"store_date">> := StoreDate}) ->
                [YearBin, _Rest] = binary:split(StoreDate, <<"-">>),
                binary_to_integer(YearBin)
              end,
    Fun = fun(Issue, Acc) ->
                  Year = GetYear(Issue),
                  if Year < Acc -> Year;
                     true -> Acc
                  end
          end,
    integer_to_binary(lists:foldl(Fun, GetYear(First), Rest)).

issue_stub(Issue) ->
    Fun = fun(Key, Acc) -> Acc#{Key => maps:get(Key, Issue, null)} end,
    lists:foldl(Fun, #{}, [<<"id">>,
                           <<"name">>,
                           <<"issue_number">>
                          ]).

% older version of erlang runnning on the server so need to provide this myself
binary_join(A, B) ->
    <<A/binary, B/binary>>.
