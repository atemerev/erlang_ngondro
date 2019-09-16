-module(orderbook).
-author("atemerev").
-include("../include/state.hrl").
-export([new_book/1, insert/2, delete/3, update/5, best/1, trim/2]).

new_book(Symbol) ->
  #orderbook{symbol = Symbol, bids = [], offers = []}.

insert(Book = #orderbook{}, Entry = #order_entry{side = Side, price = Price}) ->
  Line = get_line(Book, Side),
  Fun = case Side of
          bid -> fun(X) -> Price > X#order_entry.price end;
          offer -> fun(X) -> Price < X#order_entry.price end;
          _ -> invalid_entry_side
        end,
  Idx = fputil:find_index(Line, Fun),
  NewLine = case Idx of
              not_found -> lists:append(Line, [Entry]);
              _ -> fputil:insert_at(Line, Idx, Entry)
            end,
  put_line(Book, Side, NewLine).

delete(Book = #orderbook{}, Side, Id) ->
  Line = get_line(Book, Side),
  Idx = fputil:find_index(Line, fun(X) -> X#order_entry.id == Id end),
  NewLine = case Idx of
              not_found -> Line;
              _ -> fputil:delete_at(Line, Idx)
            end,
  put_line(Book, Side, NewLine).

update(Book = #orderbook{}, Side, Id, _, 0) -> delete(Book, Side, Id);

update(Book = #orderbook{}, Side, Id, Price, Amount) ->
  Line = get_line(Book, Side),
  IdxById = fputil:find_index(Line, fun(X) -> X#order_entry.id == Id end),

  case IdxById of
    not_found -> not_found;
    Idx ->
      Entry = lists:nth(Idx, Line),
      E1 = maybe_new(Price,Entry,price,Entry),
      E2 = maybe_new(Amount,Entry,amount,E1),
      NewLine = fputil:replace_at(Line, Idx, E2),
      put_line(Book, Side, NewLine)
  end.

trim(#orderbook{symbol = Symbol, bids = Bids, offers = Offers}, Length) ->
  #orderbook{symbol = Symbol, bids = lists:sublist(Bids, Length), offers = lists:sublist(Offers, Length)}.

maybe_new(Value,Entry,price,_)  when Value > 0 -> setelement(#order_entry.price,Entry,Value);
maybe_new(Value,Entry,amount,_) when Value > 0 -> setelement(#order_entry.amount,Entry,Value);
maybe_new(_,_,_,Default)                       -> Default.

get_line(#orderbook{bids = B}, bid)     -> B;
get_line(#orderbook{offers = O}, offer) -> O;
get_line(#orderbook{}, _)               -> invalid_entry_side.

put_line(Book, bid,   NewLine) -> setelement(#orderbook.bids,Book,NewLine);
put_line(Book, offer, NewLine) -> setelement(#orderbook.offers,Book,NewLine);
put_line(_,    _,     _)       -> invalid_entry_side.

best([])    -> not_found;
best([H|_]) -> H#order_entry.price;

best(#orderbook{bids = Bids, offers = Offers}) -> {best(Bids),best(Offers)}.
