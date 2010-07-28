import ply.lex as lex
import ply.yacc as yacc
import time, sys, traceback

                        
class OneAtomPerLine:
    def __init__(self, tokens, filepath, ext):
        self.tokens = tokens
        self.filepath = filepath
        self.ext = ext
        #extention of the new file
        self.newext = '.atoms'
        self.data = []
        
        self.build_lines()
        self.write_data()
    def __str__(self):
        data = ''
        for line in self.data:
            data += line
        return data    
    def build_lines(self):
        current_line = ''
        commentflag = 0
        data = self.data
        for token in self.tokens:
            value = token.value
            typ = token.type
            
            if (typ == "WORD") or (typ == "STRING") or\
                (typ == "CHAR") or (typ == "DIVIDE"):
                current_line = current_line.replace('\n','')
                current_line += value
            elif (typ == "ARRAY"):
                current_line = current_line.replace('\n','')
                data.append(current_line)
                current_line = ''
            elif typ == "SEMICOLON":
                current_line = current_line.replace('\n','')
                current_line += value + '\n'
                data.append(current_line)
                current_line = ''
            elif (typ == "LBRACE"):
                current_line = current_line.replace('\n','')
                data.append(current_line + '\n' + value + '\n')
                #data.append(current_line + value + '\n')
                current_line = ''
            elif (typ == "RBRACE"):
                current_line = current_line.replace('\n','')
                data.append(current_line + value + '\n')
                current_line = ''
            elif (typ == "FOR"):
                current_line += value
                current_line = current_line.replace('\n','')
                data.append(current_line + '\n')
                current_line = ''
            elif (typ == "PACKAGE") or (typ == "IMPORT"):
                current_line += value
                data.append(current_line+'\n')
                current_line = ''
            elif (typ == "COMMENT"):
                if commentflag == 0:
                    commentflag += 1
                    print "Warning: Comments should be ignored, change t_COMMENT to t_ignore_COMMENT"
            else:
                raise Exception("Given unhandled token type: " + str(typ))

    def write_data(self):
        newfilename = self.filepath + self.ext + self.newext
        f = open(newfilename, 'w')
        f.writelines(self.data)

        
def make_data(filepath):
    data = ''
    f = open(filepath, 'r')
    for line in f:
        data+= line
    f.close()
    return data

def tokenize(data, printopt=False):
    tokens = (

        "PACKAGE",
        "IMPORT",
        "COMMENT",
        "LBRACE",
        "RBRACE",
        "WORD",
        "FOR",
        "ARRAY",
        "DIVIDE",
        "STRING",
        "CHAR",
        "SEMICOLON",
        "WHITESPACE"

        )
    t_ignore = " \t\n\r"
    
    def t_ignore_COMMENT(t):
        r'(/\*(.|\n|\r|\r\n|\n\r)*?\*/)|(//[^\n]*)'
        pass
    def t_WHITESPACE(t):
        r'\n\r\t '
        return t

    def t_STRING(t):
        r'\"(\\\\\\\")?(\\\"|[^\"])*?(\\\\)?\"'
        return t
    def t_CHAR(t):
        r'\'(\\[^\\]|\\\'|[^\\]|\\\\|\\[0-9A-Za-z]+)\''
        return t
    def t_PACKAGE(t):
        r'package[^\n;]+;'
        return t
    def t_IMPORT(t):
        r'import[^\n]*;'
        return t
    def t_LBRACE(t):
        r'\{'
        return t
    def t_RBRACE(t):
        r'\}'
        return t
    
    def t_FOR(t):
        r'for[ ]*?\([^;\{]*;[^;\{]*;[^;\{]*?\)|for[ ]*?\([^;\{]+:[^;\{]\)|else[ ]+for[ ]*?\([^;\{]*;[^;\{]*;[^;\{]*?\)|else[ ]+for[ ]*?\([^;\{]+:[^;\{]\)'
        return t
    def t_DIVIDE(t):
        r'/'
        return t
    def t_ARRAY(t):
        r'([^\";])+[ ]new[\n\r\t ]+[A-Za-z][A-Za-z0-9]*[][\n\r\t ]*\{([^\*;]*?)\};'
        return t
    def t_WORD(t):
        r'([^\"\'\{\};/])+'
        return t
    t_SEMICOLON = ";"
    
    
    
    def t_error(t):
        if t.value[0] == '\"':
            raise Exception("Error: unable to tokenize double quote: \". Most likely "+
                            "something is wrong with the string regexp.")
        elif t.value[0] == '\'':
            raise Exception("Error: unable to tokenize single quote: \'. Most likely" +
                            "something is wrong with the char regexp.")
        else:
            raise Exception("Error unable to tokenize value: " + str(t.value[0]))
        #print "Unable to parse value: "+ str(t.value[0])
        t.lexer.skip(1)
        

    lexer = lex.lex()
    lexer.input(data)

    #dont think <data> is needed anymore (check)
    data = ''
    tokens = []
    if printopt == 'True':
        print "Beginning to tokenize:"
        for tok in lexer:
            print tok
            tokens.append(tok)
        print "\n####################\ndone tokenizing\n####################\n"
    elif printopt == 'False':
        for tok in lexer:
            tokens.append(tok)
    else:
        raise Exception("Given bad command line argument for printopt, must be True or False. Given: " + str(printopt))

    return tokens

def main():
    if len(sys.argv) != 4:
        raise Exception("Must supply command line arguments consisting of the filepath (without the .java) and then the extension (such as .java) to the code to be parsed. Was given:" + str(sys.argv))
    else:
        filepath = sys.argv[1]
        ext = sys.argv[2]
        printopt = sys.argv[3]
        if ext[0] == 'e':
            ext = ''
    
    data = make_data(filepath+ext)
    tokenized = tokenize(data, printopt)
    parsed = OneAtomPerLine(tokenized,filepath, ext)


main()

