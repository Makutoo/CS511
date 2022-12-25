-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
	%check if the chatroom exists yet.
	Chatroom_is_existed = maps:is_key(ChatName, State#serv_st.chatrooms),
	%look up the client’s nickname from the server’s serv st record.
	ClientNickName = maps:get(ClientPID, State#serv_st.nicks),

	case Chatroom_is_existed of 
		true ->
			ChatPID = maps:get(ChatName, State#serv_st.chatrooms),

			%send the message {self(), Ref, register, ClientPID, ClientNick}[C] to the chatroom.
			ChatPID!{self(), Ref, register, ClientPID, ClientNickName},
			Old_registrations = maps:get(ChatName, State#serv_st.registrations),

			State#serv_st{
				registrations = maps:put(ChatName, [ClientPID]++Old_registrations, State#serv_st.registrations)
			};
		false ->
			% If the chatroom does not yet exist, the server must spawn the chatroom.
			ChatPID = spawn(chatroom, start_chatroom, [ChatName]),	

			%send the message {self(), Ref, register, ClientPID, ClientNick}[C] to the chatroom.
			ChatPID!{self(), Ref, register, ClientPID, ClientNickName},

			State#serv_st{
				registrations = maps:put(ChatName, [ClientPID], State#serv_st.registrations),
			 	chatrooms = maps:put(ChatName, ChatPID, State#serv_st.chatrooms)
			}
	end.


	% ClientNick = maps:get(ClientPID, State#serv_st.nicks),
    % case maps:is_key(ChatName, State#serv_st.chatrooms) of
	% 	true ->
	% 		ChatPID = maps:get(ChatName, State#serv_st.chatrooms),
	% 		ChatPID!{self(), Ref, register, ClientPID, ClientNick},
	% 		PrevReg = maps:get(ChatName, State#serv_st.registrations),
	% 		State#serv_st{
	% 			registrations = maps:put(ChatName, [ClientPID] ++ PrevReg, State#serv_st.registrations)
	% 		};
	% 	false ->
	% 		ChatPID = spawn(chatroom, start_chatroom, [ChatName]),	
	% 		ChatPID!{self(), Ref, register, ClientPID, ClientNick},
	% 		State#serv_st{
	% 			registrations = maps:put(ChatName, [ClientPID], State#serv_st.registrations),
	% 		 	chatrooms = maps:put(ChatName, ChatPID, State#serv_st.chatrooms)
	% 		}
	% end.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
	% lookup the chatroom’s PID from the server’s state serv st.
	ChatRoomPID = maps:get(ChatName, State#serv_st.chatrooms),

	% remove the client from its local record of chatroom registrations.
	OldChatRoomRegistrations = maps:get(ChatName, State#serv_st.registrations),
	NewChatRoomRegistrations = lists:delete(ClientPID, OldChatRoomRegistrations),
	
	UpdatedState = State#serv_st{
		registrations = maps:put(ChatName, NewChatRoomRegistrations, State#serv_st.registrations)
	},

	% send the message {self(), Ref, unregister, ClientPID}[C] to the chatroom.
	ChatRoomPID!{self(), Ref, unregister, ClientPID},

	% send the message {self(), Ref, ack leave}[B] to the client.
	ClientPID!{self(), Ref, ack_leave},
	UpdatedState.

    % ChatPID = maps:get(ChatName, State#serv_st.chatrooms),
	% UpdatedRegistration = lists:delete(ClientPID, maps:get(ChatName, State#serv_st.registrations)),
    % NewState = State#serv_st{
	% 				registrations = maps:put(ChatName, UpdatedRegistration, State#serv_st.registrations)
	% 			},
    % ChatPID!{self(), Ref, unregister, ClientPID},
    % ClientPID!{self(), Ref, ack_leave},
    % NewState.



%% executes new nickname protocol from server perspective

has_element(Element, [H|T])->
	if Element == H ->
		true;
	true ->
		has_element(Element, T)
	end;

has_element(_Element, []) ->
	false.

do_new_nick(State, Ref, ClientPID, NewNick) ->
	AllNickName = maps:values(State#serv_st.nicks),
	case has_element(NewNick,AllNickName) of
		true ->
			ClientPID!{self(), Ref, err_nick_used},
			State;
		false ->
			Rooms = maps:filter(fun (_, ClientsReg) -> has_element(ClientPID, ClientsReg) end, State#serv_st.registrations),
			lists:foreach(fun(ChatRoom) ->
				PID = maps:get(ChatRoom, State#serv_st.chatrooms),

				%sending each relevant chatroom the message {self(), Ref, update nick,ClientPID, NewNick}[C] to the chatrooms.
				PID!{self(), Ref, update_nick, ClientPID, NewNick} 

			end, maps:keys(Rooms)),

			% send the message {self(), Ref, ok nick}[B] to the client.
			ClientPID!{self(), Ref, ok_nick},
			State#serv_st{
				nicks = maps:put(ClientPID, NewNick, State#serv_st.nicks)
			}
	end.


	
	% case lists:member(NewNick, maps:values(State#serv_st.nicks)) of
	% 	true ->
	% 		ClientPID!{self(), Ref, err_nick_used},
	% 		State;
	% 	false ->
	% 		CurRooms = maps:filter(fun (_, ClientsReg) -> lists:member(ClientPID, ClientsReg) end, State#serv_st.registrations),
	% 		lists:foreach(fun(ClientChatRoom) ->
	% 			PID = maps:get(ClientChatRoom, State#serv_st.chatrooms),
	% 			PID!{self(), Ref, update_nick, ClientPID, NewNick} 
	% 		end, maps:keys(CurRooms)),
	% 		ClientPID!{self(), Ref, ok_nick},
	% 		State#serv_st{
	% 			nicks = maps:put(ClientPID, NewNick, State#serv_st.nicks),
	% 			registrations = State#serv_st.registrations,
	% 			chatrooms = State#serv_st.chatrooms
	% 		}
	% end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
	% Remove client from nicknames.
	DeletedClientNickName = maps:remove(ClientPID, State#serv_st.nicks),
	UpdateState = State#serv_st{nicks = DeletedClientNickName},

	% find chatroom in which the client is registered.
	CharRoomThatClientIsRegistered = maps:filter(fun(_CN,ListOfClientPIDS) ->
										IsExisted = has_element(ClientPID, ListOfClientPIDS),
										IsExisted == true 
										end, 
										UpdateState#serv_st.registrations),
    ChatNames = maps:keys(CharRoomThatClientIsRegistered),

	%send the message {self(), Ref, unregister, ClientPID}[C] to each chatroom
	lists:map(fun(CName) ->
					ChatRoomPID = maps:get(CName,UpdateState#serv_st.chatrooms),
					ChatRoomPID!{self(), Ref, unregister, ClientPID}
					end, 
					ChatNames),

	% send the message {self(), Ref, ack quit}[B] to the client.
	ClientPID!{self(), Ref, ack_quit},

	UpdateState#serv_st{
		registrations = maps:map( 
			fun(_ChatName,PIDS) ->
				case has_element(ClientPID, PIDS) of
					true->
						PIDS -- [ClientPID];
					false->
						PIDS
				end
			end, UpdateState#serv_st.registrations)
	}.



	% UpdateState = State#serv_st{nicks = maps:remove(ClientPID, State#serv_st.nicks)},
    % ChatNames = maps:keys(maps:filter(fun(_CN,ListOfClientPIDS) ->
	% 			IsMember = lists:member(ClientPID, ListOfClientPIDS),
	% 			IsMember == true 
	% 			end, UpdateState#serv_st.registrations)),
	% lists:map(fun(CName) ->
	% 				ChatRoomPID = maps:get(CName,UpdateState#serv_st.chatrooms),
	% 				ChatRoomPID!{self(), Ref, unregister, ClientPID}
	% 			end, ChatNames),
	% ClientPID!{self(), Ref, ack_quit},
	% UpdateState#serv_st{
	% 	registrations = maps:map( 
	% 		fun(_ChatName,ListofPIDS) ->
	% 			A = lists:member(ClientPID, ListofPIDS),
	% 			if
	% 				A == true -> 
	% 					ListofPIDS -- [ClientPID];
	% 				true ->
	% 					ListofPIDS
	% 			end
	% 		end, UpdateState#serv_st.registrations)
	% }.