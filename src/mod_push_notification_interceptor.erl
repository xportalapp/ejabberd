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
    RabbitExchangeType = maps:get(rabbit_exchange_type, Opts, "direct"),
    RabbitUser = maps:get(rabbit_user, Opts, "guest"),
    RabbitPassword = maps:get(rabbit_password, Opts, "guest"),
    RabbitVHost = maps:get(rabbit_vhost, Opts, "/"),
    SslOptions = maps:get(ssl_options, Opts, none),

    % MapSslOptions = #{
    %     cacertfile => element(2, lists:nth(1, SslOptions)),
    %     certfile => element(2, lists:nth(2, SslOptions)),
    %     keyfile => element(2, lists:nth(3, SslOptions)),
    %     verify => element(2, lists:nth(4, SslOptions)),
    %     fail_if_no_peer_cert => element(2, lists:nth(5, SslOptions))
    % },

    {ok, Connection} = amqp_connection:start(
        #amqp_params_network{
            username = list_to_binary(RabbitUser), 
            password = list_to_binary(RabbitPassword),
            host = RabbitHost, 
            virtual_host = list_to_binary(RabbitVHost),
            port = RabbitPort,
            ssl_options = SslOptions
        }),
    ?INFO_MSG("RabbitMq connection opened: ~p", [Connection]),

    {ok, Channel} = amqp_connection:open_channel(Connection),
    ?INFO_MSG("RabbitMq channel opened: ~p", [Channel]),

    ets:new(my_table, [set, named_table, public]),
    ets:insert(my_table, {notif_connection, Connection}),
    ets:insert(my_table, {notif_channel, Channel}),
    ets:insert(my_table, {rabbit_exchange, list_to_binary(RabbitExchange)}),
    ets:insert(my_table, {rabbit_exchange_type, list_to_binary(RabbitExchangeType)}),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_message_to_rb, 50),
    ok.

stop(_Host) ->
    ?INFO_MSG("Bye bye, ejabberd Push Notification module down!", []),
    ok.

depends(_Host, _Opts) ->
    [].

%% Setting up module options
mod_opt_type(rabbit_port) ->
    econf:int();
mod_opt_type(rabbit_host) ->
    econf:string();
mod_opt_type(rabbit_vhost) ->
    econf:string();
mod_opt_type(rabbit_exchange) ->
    econf:string();
mod_opt_type(rabbit_exchange_type) ->
    econf:string();
mod_opt_type(rabbit_user) ->
    econf:string();
mod_opt_type(rabbit_password) ->
    econf:string();
mod_opt_type(ssl_options) ->
    econf:list(econf:any()).


mod_options(_Host) ->
    [
        {rabbit_port, 5672},
        {rabbit_host, "host.docker.internal"},
        {rabbit_vhost, "/"},
        {rabbit_exchange, "all_events"},
        {rabbit_exchange_type, "direct"},
        {rabbit_user, "guest"},
        {rabbit_password, "guest"},
        {ssl_options, none}
    ].

mod_doc() ->
    #{desc =>
          ?T("This is an example module.")}.

-spec send_message_to_rb({stanza(), c2s_state()})
      -> {stanza(), c2s_state()}.
send_message_to_rb({#message{from = From, to = To, type = Type, sub_els = SubEls} = Pkt, #{jid := JID} = C2SState}) ->
    ?INFO_MSG("Received message packet ~p", [Pkt]),

    % Get the channel connection
    [{_, Channel}] = ets:lookup(my_table, notif_channel),

    % Get the message body, set it to empty string if none
    Body = Pkt#message.body,
    EncryptedMessage = try lists:nth(1, Body) of
        {_, _, EncryptedMessageFun} -> EncryptedMessageFun
    catch _:_ -> <<"">>
    end,
    
    % Get the message type: it has to be partially-encrypted-message
    IsPartial = try lists:foreach(
        fun(#xmlel{name = <<"body-type">>, attrs = [], children = [{xmlcdata, <<"partially-encrypted-message">>}]}) ->
            ok
        end,
        SubEls
    ) catch _:_ ->
        false
    end,

    if Type == chat andalso IsPartial == ok ->
        PayloadStruct = #{
            chain => 508, 
            sendAddress => From#jid.luser,
            destinationAddress => To#jid.luser, 
            pushNotification => #{
                type => <<"newChatMessage">>,
                title => <<"You have a new message!">>,
                body => <<"Tap here for more info">>,
                data => EncryptedMessage
            },
            isSilent => false
            },
        
        % JSON encode the payload
        Payload = jiffy:encode(PayloadStruct),
        Publish = #'basic.publish'{exchange = Exchange, routing_key = <<"">>},
        amqp_channel:cast(Channel, Publish, #amqp_msg{payload=Payload});
    true -> false
    end,

    {Pkt, C2SState};
send_message_to_rb(Acc) ->
    Acc.