
import pyparsing as pp
pp.ParserElement.enablePackrat()

class Parser(object):

    def __init__(self, factory):

        self.f = factory
        self.parser = self.__create()

        return
        
    def parseString(self, s):
        try:
            r = self.parser.parseString(s)
            return r[0]
        except Exception as e:
            raise Exception("parse exception: %s:%s" % (type(e), str(e)))

            
    def __create(self):

        START = pp.StringStart().suppress()
        END   = pp.StringEnd().suppress()
        
        #----------------------------------------------------------------------#
        # LANGUAGE TOKENS                                                 
        #----------------------------------------------------------------------#
        
        TRUE         = pp.Literal('True'  )                             .setParseAction( lambda s,loc,toks: toks[0] )
        FALSE        = pp.Literal('False' )                             .setParseAction( lambda s,loc,toks: toks[0] ) 

        AND          = pp.Literal('and'   )                             .setParseAction( lambda s,loc,toks: toks[0] )
        OR           = pp.Literal('or'    )                             .setParseAction( lambda s,loc,toks: toks[0] )
        NOT          = pp.Literal('not'   )                             .setParseAction( lambda s,loc,toks: toks[0] )

        #
        # Expression's elements
        #
        LEFT_PAREN   = pp.Literal('(' )       
        RIGHT_PAREN  = pp.Literal(')' )     
        LEFT_SPAREN  = pp.Literal('[' )              
        RIGHT_SPAREN = pp.Literal(']' )              
        COMMA        = pp.Literal(',' )              
        SEMICOLON    = pp.Literal(';' )              
        
        # OID's syntax elements
        COLUMN       = pp.Literal(':' ) 
        TYPE_NEW     = pp.Literal('@' ) 
        TYPE_OLD     = pp.Literal('#' ) 
        
        # Unescaped String prefix
        UNESCAPE_STR = pp.Literal('r')      

        #
        # Operators
        #
        
        ASSIGN       = pp.Literal('=')                                           
        # OIDs concat operator
        DOT          = pp.Literal('.')    
        
        PLUS_PLUS    = pp.Literal('++')                                         
        MINUS_MINUS  = pp.Literal('--')                                       
        
        POWER        = pp.Literal('**')                                         
        
        PLUS         = pp.Literal('+')                                                
        MINUS        = pp.Literal('-')                                               
        MULTI        = pp.Literal('*')                                               
        DIV          = pp.Literal('/')                                                
        MOD          = pp.Literal('%')                                                
           
        EQ           = pp.Literal('eq')                                 
        EQUAL        = pp.Literal('==')                                            
        NEQUAL       = pp.Literal('!=')                                            
           
        REGEXPQUAL   = pp.Literal('=~')           
           
        GT           = pp.Literal('>')            
        LT           = pp.Literal('<')            
        GEQ          = pp.Literal('>=')           
        LEQ          = pp.Literal('<=')           
             
        LOGIC_NOT    = pp.Literal('!')            
        LOGIC_AND    = pp.Literal('&&')          
        LOGIC_OR     = pp.Literal('||')          
          
        BITAND       = pp.Literal('&')           
        BITOR        = pp.Literal('|')            
        BITXOR       = pp.Literal('^')            
        
        # One's complement operator
        BITONE       = pp.Literal('~')           
                     
        IF           = pp.Literal('if')         
        THEN         = pp.Literal('then')        
        ELSE         = pp.Literal('else')        
                     
        TRY          = pp.Literal('try')         
        CATCH        = pp.Literal('catch')       

        #---------------------------------------------------------------------------*/
        #  Language Types
        #---------------------------------------------------------------------------*/
        
        #
        # Literals
        #
        
        QUOTED  = pp.QuotedString('"', escChar='\\') | pp.QuotedString("'", escChar='\\')
        
        STRING  =  pp.originalTextFor(QUOTED) 
        
        RSTRING = pp.originalTextFor(UNESCAPE_STR + QUOTED)
        
        #
        # Variable identifiers ($a, $a1, $_a,  $a_a123)
        #
        VAR_ID  = pp.Word('$', pp.alphanums+'_', min=2)
        
        #
        # Function identifiers
        #        
        FUNCTION_ID = pp.Word(pp.alphas, pp.alphanums+'_', min=1) 
        
        #
        # Numbers
        #        
        HEX    = pp.originalTextFor(pp.Regex('[0][xX][0-9a-fA-F]+'))
        
        DEC    = pp.originalTextFor(pp.Word('0') | pp.Regex('[1-9][0-9]*'))
        
        OCTAL  = pp.originalTextFor(pp.Regex('[0][0-7]+'))
                
        FLOAT1 = pp.Regex('[0-9]+[\.][0-9]+([eE][+-]?[0-9]+)*')
        
        FLOAT2 = pp.Regex('[0-9]+[\.]([eE][+-]?[0-9]+)*')
        
        FLOAT  = pp.originalTextFor(FLOAT1 | FLOAT2)
        
        #
        # Special identifiers  { <name> (@|#) }
        #
        DATA_ID = pp.originalTextFor(pp.Combine(
            pp.Word('{') 
            + 
            pp.Word(pp.alphas, pp.alphanums+'_-.') 
            + 
            pp.Word('@#') 
            + 
            pp.Word('}') 
        ))
        
        #----------------------------------------------------------------------#
        #----------------------------------------------------------------------#
        #                                                                      
        # GRAMMAR SYNTAX                                                       
        #                                                                      
        #----------------------------------------------------------------------#
        #----------------------------------------------------------------------#

        #----------------------------------------------------------------------#
        #  variabile
        #  constants    (1, 1.0, 'c', "foo", ecc...)
        #  ( ... )     
        #----------------------------------------------------------------------#
        
        OID_SEQUENCE = pp.Regex('[0-9]+[\.][0-9]+([\.][0-9]+)+')                

        constant = (
              TRUE                                                      .setParseAction( lambda s, loc, toks: self.f.createBool   ( True             ) ) 
            | FALSE                                                     .setParseAction( lambda s, loc, toks: self.f.createBool   ( False            ) ) 
            | HEX                                                       .setParseAction( lambda s, loc, toks: self.f.createInteger( int(toks[1], 16) ) )
            | (~(OID_SEQUENCE) + FLOAT)                                 .setParseAction( lambda s, loc, toks: self.f.createFloat  ( float(toks[0])   ) )
            | OCTAL                                                     .setParseAction( lambda s, loc, toks: self.f.createInteger( int(toks[1],  8) ) )
            | DEC                                                       .setParseAction( lambda s, loc, toks: self.f.createInteger( int(toks[1], 10) ) )
            | STRING                                                    .setParseAction( lambda s, loc, toks: self.f.createString ( toks    ,True    ) )
            | RSTRING                                                   .setParseAction( lambda s, loc, toks: self.f.createString ( toks[1:],True    ) )
        )
                
        cond_expr = pp.Forward()

        #----------------------------------------------------------------------#
        # Primary Expr
        #----------------------------------------------------------------------#
        
        primary_expr = (
            (LEFT_PAREN.suppress() + cond_expr + RIGHT_PAREN.suppress())    .setParseAction( lambda s, loc, toks: toks[0] )
            | 
            VAR_ID                                                          .setParseAction( lambda s, loc, toks: self.f.createIdentifier( toks[0] ) ) 
            | 
            DATA_ID                                                         .setParseAction( lambda s, loc, toks: self.f.createDataIdentifier( toks[1] ) ) 
            | 
            constant
        )                                                                       

        #----------------------------------------------------------------------#
        # POSTFIX EXPRESSION
        #----------------------------------------------------------------------#
        # foo()
        # for(a,b,...)
        # $id()
        # $id
        # $id(a,b,...)
        #----------------------------------------------------------------------#

        #
        # Named argument
        #        
        named_argument_value = pp.Forward() 
        
        name_argument = (
            FUNCTION_ID
            +
            ASSIGN.suppress()
            +
            named_argument_value
        )                                                               .setParseAction( lambda s, loc, toks: self.f.createNamedArgument(toks[0], toks[1]) )
        
        #
        # Simple argument
        #
        simple_argument_value = pp.Forward()                                       
        
        #
        # 1, 2, 3, foo=10, bar=10234
        #
        argument = name_argument | simple_argument_value                                                 
                                                                           
        argument_expr_list = (argument + pp.ZeroOrMore(COMMA.suppress() + argument)) 
        
        #----------------------------------------------------------------------#
        #  ( ), (a,b,c,...)
        #----------------------------------------------------------------------#

        def _call_expr_callback(s, loc, toks):
            args = toks.get('args')
            if args is None: args = []
            else           : args = list(args)
            return ('CALL', args)

        call_expr = (
            LEFT_PAREN.suppress() 
            + 
            pp.Optional(argument_expr_list('args')) 
            + 
            RIGHT_PAREN.suppress()
        )                                                               .setParseAction( _call_expr_callback )
        
        #----------------------------------------------------------------------#
        # [], [;], [i], [i;], [;j]   [i;j]
        #----------------------------------------------------------------------#
        
        def _range_expr_callback(s, loc, toks):
            args = []
            start = toks.get('start')
            args.append(start)
            if 'end' in toks:
                end  = toks.get('end'  )
                args.append(end)
            return ('RANGE', args )
        
        range_value = pp.Forward() 
        
        range_expr = (
            LEFT_SPAREN.suppress()
            +
            pp.Optional(range_value)('start')
            +          
            pp.Optional(
                SEMICOLON.suppress()
                +
                pp.Optional(range_value)('end')
            )                                                                   
            +
            RIGHT_SPAREN.suppress()                                 
        )                                                               .setParseAction( _range_expr_callback )   
        
        #----------------------------------------------------------------------#

        call_or_range = range_expr | call_expr 
        
        def  _func_callback(s, loc, toks):

            if len(toks) == 1: 
                return toks[0] 
            
            current_t = toks[0]
            
            for t in toks[1:]:
                f_type, args = t
                
                if f_type == 'CALL':
                    current_t = self.f.createCallOp(current_t, args)
                elif f_type == 'RANGE':
                    current_t = self.f.createRangeOp(current_t, args)
                else:
                    raise Exception("ERROR")

            return current_t
        
        postfix_expr = (
            (
             FUNCTION_ID + pp.OneOrMore(call_or_range)
            )                                                        .setParseAction( _func_callback )
            |
            (
             primary_expr + pp.ZeroOrMore(call_or_range)
            )                                                        .setParseAction( _func_callback )
        )
        
        #----------------------------------------------------------------------#
        #  UNARY EXPRESSION
        #----------------------------------------------------------------------#
        #  <expr>
        #  <expr>()
        #  <expr>[]
        #  + <expr> 
        #  - <expr> 
        #  ~ <expr>
        #  ! <expr>
        #---------------------------------------------------------------------------*/
        
        unary_expr = pp.Forward()
        
        calc_expr = (
            postfix_expr                
            | 
            ( PLUS_PLUS       .suppress() + unary_expr )                .setParseAction( lambda s, loc, toks: self.f.createAddAddOp(toks[0])) 
            |                                                           
            ( MINUS_MINUS     .suppress() + unary_expr )                .setParseAction( lambda s, loc, toks: self.f.createSubSubOp(toks[0])) 
            |                                                           
            ( PLUS            .suppress() + unary_expr )                .setParseAction( lambda s, loc, toks: toks[0] ) 
            |                                                           
            ( MINUS           .suppress() + unary_expr )                .setParseAction( lambda s, loc, toks: self.f.createMinusOp (toks[0])) 
            |                                                                                                                      
            ( (LOGIC_NOT|NOT) .suppress() + unary_expr )                .setParseAction( lambda s, loc, toks: self.f.createNotOp   (toks[0])) 
            |                                                           
            ( BITONE          .suppress() + unary_expr )                .setParseAction( lambda s, loc, toks: self.f.createBitOneOp(toks[0])) 
        )
        
        #---------------------------------------------------------------------------*/
        # OID Expressions
        #---------------------------------------------------------------------------*/
        # These expressions rappresent SNMP OID values: 
        #
        #    <oid expression>  [':' <community-expr>] '@' [ <host-expr> [':' <port-expr>] ] 
        #
        # where <oid expression> is:
        #
        #    n.n.n '.' <exp-1> '.' <exp-2> '.' <exp-n>
        #
        #---------------------------------------------------------------------------*/
        
        #
        #  The DOT ('.') operator is a bit tricky: expressions are converted 
        #  into strings and concatenated.
        #
        #  This means that if i concatenate OID  1.2.3.4  with the float 
        #  literal 5.6  the result is  1.2.3.4.5.6
        #
        
        def _oid_compositon_callback(s, loc, toks):
            toks = list(toks)
            
            expr = toks.pop(0)
            while toks:
                expr = self.f.createConcatOID(expr, toks.pop(0) )
            return expr
            
        def _oid_callback(s, loc, toks):
            return self.f.createOID(toks[1])
        
        oid_compositon = (
            pp.originalTextFor(OID_SEQUENCE)                            .setParseAction( _oid_callback ) 
            +
            pp.ZeroOrMore(
                DOT.suppress()
                +
                ( 
                    pp.originalTextFor(OID_SEQUENCE)                    .setParseAction( _oid_callback ) 
                    |
                    postfix_expr
                )
            )                                                           
        )                                                               .setParseAction( _oid_compositon_callback )
        
        def _snmp_single_expr_callback(s, loc, toks):
            oid       = toks['oid']
            community = toks['community']  if 'community' in toks else None
            t         = toks['type']
            node      = toks['node']       if 'node'      in toks else None
            port      = toks['port']       if 'port'      in toks else None
            return self.f.createSnmpValue(oid, community, t, node, port)
        
        snmp_single_expr = (
            oid_compositon                                              ('oid')       
            +
            pp.Optional(COLUMN.suppress() + postfix_expr)               ('community')
            +  
            pp.originalTextFor(TYPE_OLD | TYPE_NEW)                     ('type')   
            +
            pp.Optional(
                postfix_expr                                            ('node')
                +
                pp.Optional(COLUMN.suppress() + postfix_expr)           ('port')
            )                                                           
        )                                                               .setParseAction( _snmp_single_expr_callback )

        #----------------------------------------------------------------------#
        # 1.3.6.1.2.1.1@ [ ]
        #----------------------------------------------------------------------#
        
        def _func_callback_x(s, loc, toks):
            toks = list(toks)
            if len(toks) == 1: return toks[0]
            expr       = toks[0]
            range_args = toks[1][1]
            return self.f.createRangeOp(expr, range_args)
            
        snmp_value_expr = (snmp_single_expr + pp.Optional(range_expr))    .setParseAction( _func_callback_x )                                   
            
        #----------------------------------------------------------------------#
        # IF <expr> THEN <expr ELSE <expr>
        #----------------------------------------------------------------------#

        def _if_callback(s, loc, toks):
            e1 = toks.get('e1')
            e2 = toks.get('e2')
            e3 = toks.get('e3')
            return self.f.createIf(e1, e2, e3)
        
        if_expr = (
            IF.suppress()
            +
            cond_expr("e1")
            + 
            THEN.suppress()
            +
            cond_expr("e2") 
            +
            ELSE.suppress()
            + 
            cond_expr("e3")
        )                                                               .setParseAction( _if_callback  )

        #----------------------------------------------------------------------#
        # try <expr> catch [ <id> ] ( <expr> ) [ catch <id> ( <expr> ) ....]
        #----------------------------------------------------------------------#
        
        def _catch_expr_callback(s, loc, toks):
            ex_name = toks.get('exception')
            expr    = toks.get('expr')

            return (ex_name, expr)
        
        def _try_expr_callback(s, loc, toks):
            body = toks['body']
            catch_list = list(toks['catch_list'])

            return self.f.createTry(body, catch_list)

        #
        # catch [ <expr> ] ( <expr> )
        #
        catch_expr_body = pp.Forward()
        
        catch_expr = (
            pp.Optional(FUNCTION_ID)('exception') 
            +
            LEFT_PAREN.suppress()
            +
            pp.Optional(cond_expr)('expr')
            +
            RIGHT_PAREN.suppress()
        )                                                               .setParseAction( _catch_expr_callback )
        
        #
        # try <expr> [ catch <expr> ( <expr> ) .... ]
        #
        catch_list = CATCH.suppress() + pp.OneOrMore( catch_expr )
        
        try_expr = (
            TRY.suppress() 
            +
            cond_expr('body')
            + 
            catch_list('catch_list')
        )                                                               .setParseAction( _try_expr_callback )
        
        #----------------------------------------------------------------------#
        # UNARY EXPRESSION
        #----------------------------------------------------------------------#
        
        unary_expr <<= (
            if_expr         
            | 
            try_expr        
            | 
            snmp_value_expr 
            | 
            calc_expr
        )

        #----------------------------------------------------------------------#
        # OPERATORS
        #----------------------------------------------------------------------#

        OP_MAP = {
            str(POWER     .match): self.f.createPowerOp,

            str(MULTI     .match): self.f.createMultiOp,
            str(DIV       .match): self.f.createDivOp,
            str(MOD       .match): self.f.createModOp,

            str(PLUS      .match): self.f.createAddOp,
            str(MINUS     .match): self.f.createSubOp,

            str(LT        .match): self.f.createLtOp,
            str(GT        .match): self.f.createGtOp,
            str(LEQ       .match): self.f.createLEqOp,
            str(GEQ       .match): self.f.createGEqOp,

            str(EQUAL     .match): self.f.createEqOp,
            str(EQ        .match): self.f.createEqOp,
            str(NEQUAL    .match): self.f.createNotEqOp,
            str(REGEXPQUAL.match): self.f.createRegExpEqOp,

            str(BITAND    .match): self.f.createBitAndOp ,
            str(BITXOR    .match): self.f.createBitXOrOp ,
            str(BITOR     .match): self.f.createBitOrOp ,

            str(AND       .match): self.f.createAndOp ,
            str(LOGIC_AND .match): self.f.createAndOp ,

            str(OR        .match): self.f.createOrOp ,
            str(LOGIC_OR  .match): self.f.createOrOp ,
        }

        def _op_callback(s, loc, toks):
            l = list(toks)
            if len(l) == 1: return l

            expr = l.pop(0)
            while l:
                op, expr2 = l.pop(0), l.pop(0)
                op_callback = OP_MAP[op]
                expr = op_callback(expr, expr2)
            return expr


        expr = unary_expr       

        #// a ** b 
        expr = ( expr + pp.ZeroOrMore( POWER                        + expr) )    .setParseAction( _op_callback )

        #// a * b                                                                                
        #// a / c                                                                                
        #// a % c                                                                                
        expr = ( expr + pp.ZeroOrMore( (MULTI|DIV|MOD)              + expr) )    .setParseAction(_op_callback )

        #// a + b                                                                                
        #// a - b                                                                                
        expr = ( expr + pp.ZeroOrMore( (PLUS|MINUS)                 + expr) )    .setParseAction(_op_callback )

        #// a < b                                                                                
        #// a > b                                                                                
        #// a <= b                                                                               
        #// a >= b                                                                               
        expr = ( expr + pp.ZeroOrMore( (LT|GT|LEQ|GEQ)              + expr) )    .setParseAction(_op_callback )

        #// a == b                                                                               
        #// a != b                                                                               
        #// a ~= b                                                                               
        expr = ( expr + pp.ZeroOrMore( (EQUAL|EQ|NEQUAL|REGEXPQUAL) + expr) )    .setParseAction(_op_callback )

        #// a & b                                                                                
        expr = ( expr + pp.ZeroOrMore( BITAND                       + expr) )    .setParseAction(_op_callback )

        #// a ^ b                                                                                
        expr = ( expr + pp.ZeroOrMore( BITXOR                       + expr) )    .setParseAction(_op_callback )

        #// a | b                                                                                
        expr = ( expr + pp.ZeroOrMore( BITOR                        + expr) )    .setParseAction(_op_callback )

        #// a && b                                                                               
        expr = ( expr + pp.ZeroOrMore( (LOGIC_AND| AND)             + expr) )    .setParseAction(_op_callback )

        #//  a || b                                                                              
        expr = ( expr + pp.ZeroOrMore( (LOGIC_OR | OR )             + expr) )    .setParseAction(_op_callback )

        #----------------------------------------------------------------------#
        # Recursive rules
        #----------------------------------------------------------------------#
        
        cond_expr <<= expr 
        
        simple_argument_value <<= cond_expr
        named_argument_value  <<= cond_expr
        range_value           <<= cond_expr
                
        #----------------------------------------------------------------------#
        # Initiali RULE                                                        
        #----------------------------------------------------------------------#
        
        lang_expr = (START + cond_expr + END)
        
        return lang_expr


