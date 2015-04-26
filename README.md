# object-lang
an assignment. sort of works

(object-interpreter
"let factObj = extend EmptyObj with
public fact = proc (n) if =(n, 0)
then 1
else *(n, (self.fact -(n, 1))) end
end ;
in (factObj.fact 5)
end")
