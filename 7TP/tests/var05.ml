var x:bool;
var i:int;
x := true;
i := 5;
while x begin
    i := i-1;
    x := if i != 0 then true else false;
end
return i;