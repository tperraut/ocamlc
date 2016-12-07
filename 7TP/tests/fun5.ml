fun f(x:int):int begin
  var y:int;
  y := x + x; 
  begin var z:int; z := y * 3; print z; end
    var x:int;
  x := 5 + y;
  return x;
end

print f(1);
