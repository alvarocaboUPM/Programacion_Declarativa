trabaja(homer, Dia) :- (Dia = lunes ; Dia = viernes), (hay_donuts(Dia) ; vigila_smithers(homer, Dia)).
trabaja(lenny, Dia) :- trabaja(carl, Dia).
trabaja(carl, Dia) :- vigila_smithers(carl, Dia).

vigila_smithers(homer, Dia) :- (Dia = lunes ; Dia = viernes).
vigila_smithers(lenny, Dia) :- (Dia = lunes ; Dia = viernes).
vigila_smithers(carl, Dia) :- (Dia = martes ; Dia = jueves).

hay_donuts(Dia) :- (Dia = lunes ; Dia = jueves).

reactor_estable(Dia) :- trabaja(homer, Dia), trabaja(lenny, Dia).
