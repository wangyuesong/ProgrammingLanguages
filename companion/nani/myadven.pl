room(kitchen).
room(office).
room(hall).
room('dining room').
room(cellar).

location(desk, office).
location(apple, kitchen).
location(flashlight, desk).
location('washing machine', cellar).
location(nani, 'washing machine').
location(broccoli, kitchen).
location(crackers, kitchen).
location(computer, office).

door(office, hall).
door(kitchen, office).
door(hall, 'dining room').
door(kitchen, cellar).
door('dining room', kitchen).

edible(apple).
edible(crackers).

tastes_yucky(broccoli).

turned_off(flashlight).
here(kitchen).

connect(X,Y) :- door(X,Y).
connect(X,Y) :- door(Y,X).

list_things(Place) :-
  location(X, Place),
  tab(2),
  write(X),
  nl,
  fail.
list_things(_).


list_connections(Place) :-
  connect(Place, X),
  tab(2),
  write(X),
  nl,
  fail.
list_connections(_).

look :-
  here(Place),
  write('You are in the '), write(Place), nl,
  write('You can see:'), nl,
  list_things(Place),
  write('You can go to:'), nl,
  list_connections(Place).

look_in(Place) :-
  write('You are in the '), write(Place), nl,
  write('You can see:'), nl,
  list_things(Place),
  write('You can go to:'), nl,
  list_connections(Place).

is_contained_in(T1, T2) :-
  location(T1, T2).

is_contained_in(T1, T2) :-
  location(T1, X),
  is_contained_in(X, T2).


factorial(0, Y) :-
  Y is 1.

factorial(X, Y) :-
  X > 0,
  X1 is X - 1,
  factorial(X1, Result),
  Y is Result * X.


location_s(object(candle, red, small, 1), kitchen).
location_s(object(apple, red, small, 1), kitchen).
location_s(object(apple, green, small, 1), kitchen).
location_s(object(table, blue, big, 50), kitchen).


list_things_s(Place) :-
  location_s(object(Thing, Color, Size, Weight),Place),
  write('A '),write(Size),tab(1),
  write(Color),tab(1),
  write(Thing), write(', weighing '),
  write(Weight), write(' pounds'), nl,
  fail.
