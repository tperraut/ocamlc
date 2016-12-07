var x:int;
x := 42;
while x != 0 begin
  var y:int;
  y := x / 2;
  print (x - 2 * y);
  x := y;
end
