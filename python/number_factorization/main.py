ff = lambda n:(p:=[next(i for i in range(2,n+1) if n % i ==0)] if n>1 else [])+(ff(n//p[0]) if p else [])
