-module(client).
-export([add_book_server/4, list_books_server/1, books/2]).
%%Mensagens de lookup:
%%
%%- livros: dado um número de cartão de cidadão determina a lista de livros requisitada por essa pessoa;
%%- empréstimos: dado o título de um livro determina a lista de pessoas que requisitaram esse livro;
%%- requisitado: dado o código de um livro determina se o livro está requisitado (retorna um booleano);
%%- códigos: dado o título de um livro retorna a lista de códigos de livros com esse título;
%%- numRequisicões: dado um número de cartão de cidadão retorna o número de livros requisitados por essa pessoa;
%%
%%Mensagens de update:
%%
%%- requisição: dados os dados de uma pessoa e o código de um livro acrescenta o par {pessoa, livro} à base de dados;
%%- retorno: dado um número de cartão de cidadão e o código de um livro retira o par respectivo da base de dados;


%%rpc:call(nome_da_maquina, modulo, função, argumentos)

books(Server, IdNumber) ->
	Server ! {self(), my_books},
	receive
		{Server, Response} ->
			Response
	end.

list_books_server(Server) ->
	Server ! {self(), list_books},
	receive
		{Server, Response} ->
			Response
	end.

add_book_server(Server,Isbn, Name, Authors) ->
	Server ! {self(), {add_book,Isbn, Name, Authors}},
	receive
		{Server, Response} ->
			Response
	end.