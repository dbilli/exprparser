
import unittest

from exprparser.parser import Parser
from exprparser.factory import BaseExprFactory, SnmpValueExprTree
from exprparser.Types import SnmpValueType

#----------------------------------------------------------------------#
#                                                                      #
#----------------------------------------------------------------------#

class TestSnmpValueType(SnmpValueType):

    def effective_snmpget(self): 
        return 666

    def effective_snmpwalk(self):
        return [ ('1.3.6.1.2.1.1.1.0', "Linux") ]

#----------------------------------------------------------------------#
#                                                                      #
#----------------------------------------------------------------------#

class TestSnmpValueExprTree(SnmpValueExprTree):
    def calculateSnmpValue(self, context, oid,community,ctype,host,port):
        return TestSnmpValueType(oid,community,ctype,host,port)

class TexstExprFactory(BaseExprFactory):
    def createSnmpValue(self, exprOID, eCommunity, charType, eHost, ePort):
        return TestSnmpValueExprTree(exprOID, eCommunity, charType, eHost, ePort)

#----------------------------------------------------------------------#
#                                                                      #
#----------------------------------------------------------------------#

test_factory = TexstExprFactory()
test_parser = Parser(test_factory)

class TestStringMethods(unittest.TestCase):

    def test_expr(self):
   
        parsed_tree = test_parser.parseString('(((3 * 1) + 3) / 2) ** 3')
        
        v = parsed_tree.evaluate({})

        self.assertEqual(v, 27)


    def test_get(self):
   
        parsed_tree = test_parser.parseString('1.3.6.1.1.1.1.0@ + 1')
        
        v = parsed_tree.evaluate({})

        self.assertEqual(v, 667)

    def test_walk(self):
   
        parsed_tree = test_parser.parseString('(1.3.6.1.1.1.1.0@[0])[1] =~ "Linux"')
        
        v = parsed_tree.evaluate({})

        self.assertTrue(v)


if __name__ == '__main__':
    unittest.main()
