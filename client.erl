-module(client).
-compile(export_all).

requisition(Server, PersonId, PersonName, Adress, MobileNumber, BookId) ->
	case(is_process_alive(Server)) of
		true ->
			Server ! {self(), {requisition, PersonId, PersonName, Adress, MobileNumber, BookId}},
			receive
				{Server,Response} ->
					Response	
			end;
		false ->
			io:format("Pid not alive ~n")
	end.

return(Server, BookId, PersonId) ->
	case(is_process_alive(Server)) of
		true ->
			Server ! {self(), {return, BookId, PersonId}},
			receive
				{Server,Response} ->
					Response
			end;
		false ->
			io:format("Pid not alive ~n")
	end.

books(Server, PersonId) ->
	case(is_process_alive(Server)) of
		true ->	
			Server ! {self(), {books,PersonId}},
			receive
				{Server, Response} ->
				Response
			end;
		false ->
			io:format("Pid not alive ~n")
	end.

loans(Server,BookName) ->
	case(is_process_alive(Server)) of
		true ->	
			Server ! {self(), {loans,BookName}},
			receive
				{Server,Response} ->
					Response
			end;
		false ->
			io:format("Pid not alive ~n")
	end.

requested(Server, BookId) ->
	case(is_process_alive(Server)) of
		true ->	
			Server ! {self(), {requested,BookId}},
			receive
				{Server,Response} ->
					Response
			end;
		false ->
			io:format("Pid not alive ~n")
	end.

codes(Server, BookName) ->
	case(is_process_alive(Server)) of
		true ->	
			Server ! {self(), {codes,BookName}},
			receive
				{Server,Response} ->
					Response
			end;
		false ->
			io:format("Pid not alive ~n")
	end.

requests_numbers(Server, PersonId)->
	case(is_process_alive(Server)) of
		true ->	
			Server ! {self(), {requests_numbers,PersonId}},
			receive
				{Server,Response} ->
					Response
			end;
		false ->
			io:format("Pid not alive ~n")
	end.

list_table(Server, Table)->
	case(is_process_alive(Server)) of
		true ->	
			Server ! {self(), {list_table,Table}},
			receive
				{Server,Response} ->
					Response
			end;
		false ->
			io:format("Pid not alive ~n")
	end.