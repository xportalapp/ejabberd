-module(mod_push_notification_interceptor).
-behaviour(gen_mod).

%% Required by ?INFO_MSG macros
-include("logger.hrl").

%% Required by ?T macro
-include("translate.hrl").
-include("ejabberd_commands.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("mod_mam.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-type c2s_state() :: ejabberd_c2s:state().

%% gen_mod API callbacks
-export([start/2, stop/1, depends/2, mod_opt_type/1, mod_options/1, mod_doc/0, send_message_to_rb/1]).

start(Host, Opts) ->
    ?INFO_MSG("Initialising Push Notification module with options ~p.", [Opts]),
    RabbitHost = maps:get(rabbit_host, Opts, "host.docker.internal"),
    RabbitPort = maps:get(rabbit_port, Opts, 5672),
    RabbitExchange = maps:get(rabbit_exchange, Opts, "all_events"),

    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = RabbitHost, port = RabbitPort}),
    if ok ->
        ?ERROR_MSG("RabbitMq connection failed: ~s", [ok, Connection]),
        exit({error, Connection});
       true ->
        ok
    end,
    ?INFO_MSG("RabbitMq connection opened: ~s", [ok, Connection]),

    {ok, Channel} = amqp_connection:open_channel(Connection),
    ?INFO_MSG("RabbitMq channel opened: ~s", [ok, Channel]),
    
    ets:new(my_table, [set, named_table, public]),
    ets:insert(my_table, {notif_connection, Connection}),
    ets:insert(my_table, {notif_channel, Channel}),
    ets:insert(my_table, {rabbit_exchange, list_to_binary(RabbitExchange)}),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_message_to_rb, 50),
    ok.

stop(_Host) ->
    ?INFO_MSG("Bye bye, ejabberd world!", []),
    ok.

depends(_Host, _Opts) ->
    [].

%% Setting up module options
mod_opt_type(rabbit_port) ->
    econf:int();
mod_opt_type(rabbit_host) ->
    econf:string();
mod_opt_type(rabbit_exchange) ->
    econf:string().


mod_options(Host) ->
    [
        {rabbit_port, 5672},
        {rabbit_host, "host.docker.internal"},
        {rabbit_exchange, "all_events"}
    ].

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
    ?INFO_MSG("Received message packet ~p", [Pkt]),
    % Get the channel connection
    [{_, Channel}] = ets:lookup(my_table, notif_channel),
    [{_, Exchange}] = ets:lookup(my_table, rabbit_exchange),
    % Get the message body and send it to the queue, together with address
    Body = Pkt#message.body,
    {_, _, MyBinaryString} = lists:nth(1, Body),
    
    PayloadStruct = #{
        chain => 508, 
        address => JID#jid.luser, 
        pushNotification => #{
            type => <<"newChatMessage">>,
            title => <<"You have a new message!">>,
            body => <<"Tap here for more info">>,
            data => MyBinaryString
        },
        isSilent => false
        },
    
    % JSON encode the payload
    Payload = jiffy:encode(PayloadStruct),
    Publish = #'basic.publish'{exchange = Exchange, routing_key = <<"">>},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload=Payload}),
    LServer = JID#jid.lserver,
    {Pkt, C2SState};
send_message_to_rb(Acc) ->
    Acc.