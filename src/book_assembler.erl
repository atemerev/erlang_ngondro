-module(book_assembler).
-compile(export_all).

new_book(Symbol) ->
  {orderbook, Symbol, [], []}.

add(Book, bid, Price, Volume) ->
  {ok}.