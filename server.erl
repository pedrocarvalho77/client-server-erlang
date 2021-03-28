-module(server).
-import(lists, [foreach/2]).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").

-record(books, {book_id, book_name, authors}).
-record(people, {person_id, person_name, address, mobile_number}).
-record(register, {book_id, person_id}).

example_tables() ->
			[	{books, 1,     "A historia",   "Sergio"},
				{books, 123,   "A historia 2", "Paulo"},
				{books, 12,    "A historia",   "Sergio"},
				{books, 1234,  "A historia 2", "Paulo"},
				{books, 12345, "A historia 3", "Rui"},
				{people, 987654321, "Pedro Carvalho",  "Amarante", 123123123},
				{people, 123456789, "Marta Sousa",     "Amarante", 987654321},
				{people, 192837465, "Amadeu Oliveira", "Lousada",  111222333},
				{register, 1,   987654321},
				{register, 12345,  123456789},
				{register, 123, 987654321},
				{register, 1234, 123456789}
                ].

do_this_only_once() ->
	mnesia:create_schema([node()]),
   	mnesia:start(),
   	mnesia:create_table(books,    [{attributes, record_info(fields, books)}]),
	mnesia:create_table(people,   [{attributes, record_info(fields, people)}]),
	mnesia:create_table(register, [{attributes, record_info(fields, register)}]),
	mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([books,people,register], 20000),
    spawn(server, loop,[]).

loop() ->
	receive
		{Client, {requisition, PersonId, PersonName, Address, MobileNumber, BookId}} ->
			Client ! {self(), add_request(PersonId, PersonName, Address, MobileNumber, BookId)},
		loop();
		{Client, {return,BookId,PersonId}} ->
			Client ! {self(), remove_request(BookId,PersonId)},
		loop();
		{Client, {books, PersonId}} ->
			Client ! {self(), demo(books,PersonId)},
		loop();
		{Client, {loans, BookName}} ->
			Client ! {self(), demo(loans,BookName)},
		loop();
		{Client, {requested, BookId}} -> 
			Client ! {self(), demo(requested,BookId)},
		loop();
		{Client, {codes, BookName}} -> 
			Client ! {self(), demo(codes,BookName)},
		loop();
		{Client, {requests_numbers, PersonId}} -> 
			Client ! {self(), demo(requests_numbers,PersonId)},
		loop();
		{Client, {list_table, Table}} ->
			Client ! {self(), demo(list_table,Table)},
		loop();
		{Client, Other} ->
			Client ! {self(), {Other, error_call}},
		loop()
	end.

add_request(PersonId, PersonName, Address, MobileNumber, BookId) ->
		R = do(qlc:q([X || X <- mnesia:table(register), X#register.book_id == BookId])),		
		P = do(qlc:q([Y || Y <- mnesia:table(people), 
						Y#people.person_id == PersonId,
						Y#people.person_name == PersonName,
						Y#people.address == Address,
						Y#people.mobile_number == MobileNumber
						])),
		B = do(qlc:q([X || X <- mnesia:table(books), X#books.book_id == BookId])),
		case not((R == []) and (P /= []) and (B /= [])) of
			false -> 
				add_regist(BookId,PersonId);	
			true ->
				io:format("Error~n")
		end.

remove_request(BookId,PersonId) ->
	remove_regist(BookId, PersonId).

add_regist(BookId, PersonId) ->
	Row = #register{book_id=BookId,person_id=PersonId},
	F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).

remove_regist(PersonId, BookId) ->
    Oid = {register,PersonId},
    F = fun() ->
		mnesia:delete(Oid)
	end,
    mnesia:transaction(F).

demo(list_table,Table) ->
    do(qlc:q([X || X <- mnesia:table(Table)]));

demo(books,PersonId) ->
    do(qlc:q([X#register.book_id || X <- mnesia:table(register), X#register.person_id==PersonId]));

demo(loans,BookName) ->
    do(qlc:q([X#register.person_id || X <- mnesia:table(register), 
    			Y <- mnesia:table(books), 
    			X#register.book_id =:= Y#books.book_id, 
    			Y#books.book_name == BookName]));

demo(requested,BookId) ->
	B = do(qlc:q([X || X <- mnesia:table(register), X#register.book_id==BookId])),
	case (B /= []) of
    	true ->
    		true;
    	false ->
    		false
    end;

demo(codes,BookName) ->
	C = do(qlc:q([X#books.book_id || X <- mnesia:table(books), X#books.book_name==BookName])),
	io:format("~w~n",[C]);

demo(requests_numbers,PersonId) ->
	L = do(qlc:q([X#register.book_id || X <- mnesia:table(register), X#register.person_id==PersonId])),
	length(L).

clear_tables() ->
	mnesia:clear_table(books),
	mnesia:clear_table(people),
    mnesia:clear_table(register),
    F = fun() ->
        foreach(fun mnesia:write/1, example_tables())
    end,
    mnesia:transaction(F).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.