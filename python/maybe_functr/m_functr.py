from abc import ABC
from abc import abstractmethod

class Functor(ABC):
    
    @abstractmethod
    def fmap(f):
      pass
    
class Just:
    def __init__(self, a):
        self.a = a

    def __str__(self):
       return f"Just {self.a}"

class Nothing:

    def __str__(self):
       return f"Nothing"


class Maybe(Functor):
    def __init__(self, v):
        if not isinstance(v, (Nothing, Just,)):
          raise Exception('wrong constructor us Nothing or Just')
        self.v = v
    
    def fmap(self, f, fun):
        if not isinstance(fun, Maybe):
          raise Exception('use maybe')  
        if isinstance(fun.v, Nothing):   
          return fun
        if isinstance(fun.v, Just): 
          try:
            r =f(fun.v.a)                  
          except Exception:
            return Maybe(Nothing())  
          return Maybe(Just(r))
        
    def __str__(self):
       return str(self.v)


class Infix:
    def __init__(self, function):
        self.function = function
    def __ror__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))
    def __or__(self, other):
        return self.function(other)
    def __rlshift__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))
    def __rshift__(self, other):
        return self.function(other)
    def __call__(self, value1, value2):
        return self.function(value1, value2)

fmap=Infix(lambda f,fun: fun.fmap(f, fun))

fc = (Maybe(Just(3)))
#fc = (Maybe(Nothing()))
e = (lambda x: x/0) |fmap| ((lambda x: x+1) |fmap| fc)

print(e)

