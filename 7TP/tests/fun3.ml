fun f(x:int):int begin return x + x ; end

fun g(x:int, y:int):int begin return f(x) + f(f(y)); end

print g(1, 2);
