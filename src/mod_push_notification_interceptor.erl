-module(mod_push_notification_interceptor).
-behaviour(gen_mod).

%% Required by ?INFO_MSG macros
-include("logger.hrl").

%% Required by ?T macro
-include("translate.hrl").
-include_lib("xmpp/include/xmpp.hrl").
% -include("mod_muc_room.hrl").
-include("ejabberd_commands.hrl").
-include("mod_mam.hrl").
-include("amqp_client/include/amqp_client.hrl").
% -include_lib("amqp_client/src/amqp_connection.hrl").
-type c2s_state() :: ejabberd_c2s:state().
-import(string,[concat/2]). 

%% gen_mod API callbacks
-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0, send_message_to_rb/1]).

start(Host, _Opts) ->
    ?INFO_MSG("Hello, ejabberd world!", []),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "host.docker.internal", port = 5672}),
    ?INFO_MSG("CONNECTION ~s", [ok, Connection]),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    ?INFO_MSG("CHANNEL ~s", [ok, Channel]),
    ets:new(my_table, [set, named_table, public]),
    ets:insert(my_table, {notif_connection, Connection}),
    ets:insert(my_table, {notif_channel, Channel}),
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

-record(rabbit_payload, {
    address           = "guest",
    data           = "guest"
}).

-spec send_message_to_rb({stanza(), c2s_state()})
      -> {stanza(), c2s_state()}.
send_message_to_rb({#message{} = Pkt, #{jid := JID} = C2SState}) ->
    ?INFO_MSG("Received message ~s from address ~s", [Pkt#message.body, JID#jid.luser]),
    % Get the channel connection
    [{_, Channel}] = ets:lookup(my_table, notif_channel),

    % Get the message body and send it to the queue, together with address
    Body = Pkt#message.body,
    {_, _, MyBinaryString} = lists:nth(1, Body),
    StrList = [
        "{",
        "\"address\": \"", 
        binary_to_list(JID#jid.luser), 
        "\", \"data\": \"", 
        binary_to_list(MyBinaryString), 
        "\"}"
        ],
    NewString = lists:concat(StrList),
    ?INFO_MSG("MESSAGE IS string ~s", NewString),
    Payload = list_to_binary(NewString),
    Publish = #'basic.publish'{exchange = <<"all_events">>, routing_key = <<"">>},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload=Payload}),
    LServer = JID#jid.lserver,
    {Pkt, C2SState};
send_message_to_rb(Acc) ->
    Acc.


% register() ->
%         
       