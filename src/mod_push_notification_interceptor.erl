-module(mod_push_notification_interceptor).
-behaviour(gen_mod).

%% Required by ?INFO_MSG macros
-include("logger.hrl").

%% Required by ?T macro
-include("translate.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("mod_muc_room.hrl").
-include("ejabberd_commands.hrl").
-include("mod_mam.hrl").

-type c2s_state() :: ejabberd_c2s:state().

%% gen_mod API callbacks
-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0, send_message_to_rb/1]).

start(Host, _Opts) ->
    ?INFO_MSG("Hello, ejabberd world!", []),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_message_to_rb, 50),
    ok.

stop(_Host) ->
    ?INFO_MSG("Bye bye, ejabberd world!", []),
    ok.

depends(_Host, _Opts) ->
    [].

mod_options(_Host) ->
    [].

mod_doc() ->
    #{desc =>
          ?T("This is an example module.")}.

% -export([process_message/1, register/0]).

-spec send_message_to_rb({stanza(), c2s_state()})
      -> {stanza(), c2s_state()}.
send_message_to_rb({#message{} = Pkt, #{jid := JID} = C2SState}) ->
    ?INFO_MSG("Got message ~msg", [Pkt]),
    LServer = JID#jid.lserver,
    Pkt1 = xmpp:del_meta(Pkt, stanza_id),
    {Pkt1, C2SState};
send_message_to_rb(Acc) ->
    Acc.


% register() ->
%         
       