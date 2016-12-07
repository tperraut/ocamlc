fun f(x:int):int begin
  var y:int;
  y := x + x;
  var z:int;
  z := y * 3;
  return z;
end

print f(1);
