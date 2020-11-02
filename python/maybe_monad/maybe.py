from abc import ABC
from abc import abstractmethod
import typing

class Monad(ABC):
    
    @abstractmethod
    def bind(m, f):
      pass

    @abstractmethod
    def wrap(a): # aka return
      pass      

class Just:
    def __init__(self, a):
        self.a = a

    def __str__(self):
       return f"Just {self.a}"        

class Nothing:
    pass

    def __str__(self):
       return f"Nothing"
       

class Maybe(Monad):
    def __init__(self, v: typing.Union[Just,Nothing]):
        if not isinstance(v, (Nothing, Just,)):
          raise Exception('wrong constructor us Nothing or Just')
        self.v = v

    def wrap(self, a):
      return Maybe(Just(a))     

    def bind(self, m, f):
        if isinstance(m.v, Nothing):
          return Maybe(Nothing())
        if isinstance(m.v, Just): 
          r =f(m.v.a)
          if not isinstance(r, self.__class__):
            raise Exception(f'not {self.__class__} monad')  
          return f(m.v.a)
        raise Exception('not monad')  

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

bind=Infix(lambda m,f: m.bind(m,f))
#print(2 |bind| 4)

v1 = Maybe(Just(4))
v2 = Maybe(Just(5))
n = Maybe(Nothing())
v2 = n

e = v1 |bind| (lambda x: v2 |bind| (lambda y: Maybe(Just(x+y))) )

print(e)



