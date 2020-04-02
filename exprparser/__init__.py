
from .parser import Parser
from .factory import BaseExprFactory


if __name__ == "__main__":

    import time
    import sys
    
    
    test    = sys.argv[1]
    
    
    
    f = BaseExprFactory()
    
    t0 = time.time()
    
    parser = Parser(f)
    
    t1 = time.time()
    
    parsed_tree = parser.parseString(test)
    
    t2 = time.time()
    
    print("%.5f" % (t1 - t0))
    print("%.5f" % (t2 - t1))
    
    print(parsed_tree)

