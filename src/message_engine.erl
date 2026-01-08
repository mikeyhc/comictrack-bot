-module(message_engine).
-include_lib("kernel/include/logger.hrl").

-export([process/1]).

-define(EPHEMERAL_FLAG, (1 bsl 6)).

process(#{<<"id">> := InteractionId,
          <<"token">> := InteractionToken
         }) ->
    Reply = #{
              type => 4,
              data => #{
                        content => <<"pong">>,
                        flags => ?EPHEMERAL_FLAG
                       }
             },
    discord_api:interaction_callback(
      InteractionId,
      InteractionToken,
      Reply
     );
process(Msg) ->
    ?LOG_WARNING("unhandled message: ~p", [Msg]),
    ok.
