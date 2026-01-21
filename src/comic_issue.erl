-module(comic_issue).

-export([issue_number_sort/2]).
-export([full_name/1, full_name/2]).

issue_number_sort(#{<<"issue_number">> := A}, #{<<"issue_number">> := B}) ->
    binary_to_integer(A) < binary_to_integer(B).

full_name(Issue) -> full_name(Issue, #{}).

% TODO this wont truncate properly
full_name(#{<<"name">> := Name,
            <<"issue_number">> := IssueNumber,
            <<"volume">> := #{<<"name">> := VolumeName}},
          Options) ->
    SafeName = case maps:get(name_length, Options, undefined) of
                   undefined -> VolumeName;
                   Length ->
                       if size(VolumeName) > Length ->
                              Head = binary:part(VolumeName, 0, Length - 3),
                              <<Head/binary, "...">>;
                          true -> VolumeName
                       end
               end,
    <<SafeName/binary, "#", IssueNumber/binary>>.
