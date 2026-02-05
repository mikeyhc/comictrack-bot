-module(comictrack_ui).

-include("ui_prefixes.hrl").
-include("discord_ui.hrl").

-import(discord_ui, [action_row/1, button/4, string_select/3, text_display/1,
                     label/2, modal_reply/3]).

-export([volume_select_modal/2, page_controls/3, read_select/4,
         unread_issue_list/2, volume_list/2, volume_view/3]).

% limits
-define(NAME_OPTION_LENGTH, 50).
-define(MAX_RESULTS, 10).

-spec volume_select_modal(binary(), [comic_volume:volume()]) ->
    discord_ui:discord_modal().
volume_select_modal(Id, Volumes) ->
    VolumeOptions = lists:map(fun build_volume_option/1, Volumes),
    modal_reply(<<"volume_select_modal_", Id/binary>>,
                <<"Select Volume">>,
                [label(<<"Which volume">>,
                       string_select(<<"volume_name_select">>,
                                     <<"Choose...">>,
                                     VolumeOptions))]).

-spec page_controls(binary(), non_neg_integer(), non_neg_integer()) ->
    [discord_ui:discord_component()].
page_controls(Prefix, Page, MaxPage) ->
    BinPage = integer_to_binary(Page),
    BinMaxPage = integer_to_binary(MaxPage),
    PageFooter = <<"(page ", BinPage/binary, " of ",
                   BinMaxPage/binary, ")">>,
    PrevPageBin = integer_to_binary(Page - 1),
    NextPageBin = integer_to_binary(Page + 1),
    PrevPageLink = <<Prefix/binary, PrevPageBin/binary>>,
    NextPageLink = <<Prefix/binary, NextPageBin/binary>>,
    [text_display(PageFooter),
     action_row([button(?BUTTON_PRIMARY,
                        <<"Previous Page">>,
                        PrevPageLink,
                        #{disabled => Page =:= 1}),
                 button(?BUTTON_PRIMARY,
                        <<"Next Page">>,
                        NextPageLink,
                        #{disabled => Page >= MaxPage})
                ])].

read_select(PagePrefix, Issues, ReadIssues, Page) ->
    Builder = fun(I=#{<<"id">> := Id,
                      <<"volume">> := #{<<"id">> := VolumeId}}) ->
                      Cid = <<?ISSUE_READ_PREFIX, VolumeId/binary, "_",
                              Id/binary>>,
                      Read = sets:is_element(Id, ReadIssues),
                      Default = if Read -> <<"Read">>;
                                   true -> <<"Unread">>
                                end,
                      [text_display(comic_issue:full_name(I)),
                       action_row([string_select(Cid,
                                                 Default,
                                                 read_options())
                                  ])]
              end,
    Truncated = lists:sublist(Issues, (Page - 1) * ?MAX_RESULTS + 1,
                              ?MAX_RESULTS),
    Reply0 = lists:flatten(lists:map(Builder, Truncated)),
    if length(Issues) > ?MAX_RESULTS ->
           Prefix = <<?ISSUE_READ_PAGE_PREFIX, PagePrefix/binary>>,
           Reply0 ++ page_controls(Prefix, Page, count_pages(Issues));
       true -> Reply0
    end.

unread_issue_list(Issues, Page) ->
    paginated_list(<<"The following issues are unread:">>,
                   Issues,
                   fun comic_issue:full_name/1,
                   <<?ISSUE_PAGE_PREFIX>>,
                   Page).

volume_list(Volumes, Page) ->
    paginated_list(<<"You are tracking the following volumes:">>,
                   Volumes,
                   fun comic_volume:full_name/1,
                   <<?VOLUME_PAGE_PREFIX>>,
                   Page).

-spec volume_view(comic_volume:volume(), sets:set(), non_neg_integer()) ->
    [discord_ui:discord_component()].
volume_view(#{<<"name">> := Name,
              <<"id">> := VolumeId,
              <<"issues">> := Issues,
              <<"start_year">> := StartYear
             },
            ReadIssues,
            Page) ->
    StartYearBin = if StartYear =/= null ->
                          <<" [", StartYear/binary, "]">>;
                      true ->
                          <<>>
                   end,
    Header = <<Name/binary, StartYearBin/binary, " (",
               VolumeId/binary, ")">>,
    RenderIssue = fun(#{<<"issue_number">> := IssueNumber,
                        <<"name">> := IssueName,
                        <<"id">> := IssueId
                       }) ->
                          Base = <<Name/binary, " #", IssueNumber/binary>>,
                          Named = if IssueName =/= null ->
                                         <<Base/binary, ": ",
                                           IssueName/binary>>;
                                     true -> Base
                                  end,
                          case sets:is_element(IssueId, ReadIssues) of
                              true -> <<Named/binary, "   :closed_book:">>;
                              false -> <<Named/binary, "   :book:">>
                          end
                  end,
    paginated_list(Header, Issues, RenderIssue,
                   <<?VOLUME_VIEW_PAGE_PREFIX, VolumeId/binary, "_">>,
                   Page).

% helper methods
build_volume_option(Volume=#{<<"id">> := Id}) ->
    #{label => comic_volume:full_name(Volume,
                                      #{name_length => ?NAME_OPTION_LENGTH}),
      value => Id
     }.

count_pages(Input) ->
    round(math:ceil(length(Input) / ?MAX_RESULTS)).

read_options() ->
    [#{label => <<"Read">>,
       value => <<"read">>
      },
     #{label => <<"Unread">>,
       value => <<"unread">>
      }
    ].

paginated_list(Title, Entries, Formatter, PagePrefix, Page) ->
    Truncated = lists:sublist(Entries, (Page - 1) * ?MAX_RESULTS + 1,
                              ?MAX_RESULTS),
    Fold = fun(Entry, Acc) ->
                   EntryBinary = Formatter(Entry),
                   <<Acc/binary, "\n* ", EntryBinary/binary>>
           end,
    Content = lists:foldl(Fold, Title, Truncated),
    [text_display(Content)|
     if length(Entries) > ?MAX_RESULTS ->
            page_controls(PagePrefix, Page, count_pages(Entries));
        true -> []
     end].
