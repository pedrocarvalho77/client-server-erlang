-module(server).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").

-record(books, {isbn, name, authors}).
-record(people, {id_number, name, adress, mobile_number}).
-record(register, {} ).

initialize() ->
	mnesia:create_schema([node()]),
   	mnesia:start(),
   	mnesia:create_table(books,  [{attributes, record_info(fields, books)}]),
	mnesia:create_table(people, [{attributes, record_info(fields, people)}]),
	mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([books,people], 20000).

run() -> 
	spawn(server, loop,[]).

loop() ->
	receive
		{Client, {add_book, Isbn, Name, Authors}} -> 
			Client ! {self(), add_book(Isbn, Name, Authors)},
		loop();
		{Client, list_books} -> 
			Client ! {self(), demo(select_book)},
		loop();
		{Client, {my_books, IdNumber}} ->
			Client ! {self(), ok}
	end.

add_book(Isbn, Name, Authors) ->
    Row = #books{isbn=Isbn, name=Name, authors=Authors},
    F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).

remove_book(Isbn) ->
    Oid = {books, Isbn},
    F = fun() ->
		mnesia:delete(Oid)
	end,
    mnesia:transaction(F).

clear_tables() ->
    mnesia:clear_table(books).

demo(select_book) ->
    do(qlc:q([X || X <- mnesia:table(books)]));

demo(select_some) ->
    do(qlc:q([{X#books.isbn, X#books.name} || X <- mnesia:table(books)]));

demo(select_books_person) ->
    do(qlc:q([{X#books.isbn, X#books.name} || X <- mnesia:table(books)]));

demo(reorder) ->
    do(qlc:q([X#books.name || X <- mnesia:table(books),X#books.isbn < 250])).

%% SQL equivalent
%%   SELECT shop.item
%%   FROM shop, cost
%%   WHERE shop.item = cost.name
%%     AND cost.price < 2
%%     AND shop.quantity < 250

%%demo(join) ->
%%    do(qlc:q([X#books.item || X <- mnesia:table(books),
%%			     X#books.quantity < 250,
%%			     Y <- mnesia:table(cost),
%%			     X#books.item =:= Y#books.name,
%%			     Y#books.price < 2
%%				])).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.