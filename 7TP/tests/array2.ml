var tab:int array;
tab := int [43];
tab[0] := 1;
tab[1] := 1;
var i:int;
i := 2;
while i < 43 begin
  tab[i] := tab[i-1] + tab[i-2];
  i := i + 1;
end
print tab[42];
newline;
exit;
