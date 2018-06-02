open Types

(*[do_krazy c st] takes a command [c] and a state [st] and updates [st] according to [c].
 * Functions very similarly to do' in State module but returns the appropriate state for if 
 * something special such as bomb or switching planes occured*)
val do_krazy: command -> state -> state
