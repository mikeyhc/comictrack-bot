-module(comic_repository_backend).

-export_type([volume_filter/0]).

-type volume_filter() :: #{id := non_neg_integer(),
                           name := binary()}.
-type volume_results() :: {ok, comic_volume:comic_volume()} |
                          {error, not_found} |
                          {error, {multiple_results,
                                   [comic_volume:comic_volume()]}}.
-type id_lookup() :: #{binary() := non_neg_integer()} | non_neg_integer().

-callback get_volume(volume_filter()) -> volume_results().
-callback get_issues() -> [comic_issue:comic_issue()].
-callback lookup_volume(volume_filter()) -> [comic_volume:comic_volume()].
-callback fetch_volume(id_lookup()) -> {ok, comic_volume:comic_volume()}.
