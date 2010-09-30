import ply.lex as lex
import ply.yacc as yacc
import time, sys, traceback

#enum { stuff, stuff2 } not handled correctly
#array newlines are not counted correctly                        
class OneAtomPerLine:
    def __init__(self, tokens, filepath, ext, printoption):
        self.tokens = tokens
        self.filepath = filepath 
        self.ext = ext
        self.printoption = printoption
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
        arraylevel = 0
        newlineno = ''
        for token in self.tokens:
            value = token.value
            typ = token.type
            lineno = str(token.lineno) 
            if ((typ == "WORD") or (typ == "STRING") or
                (typ == "CHAR") or (typ == "DIVIDE") or
                (typ == "ARRAY")):
                if newlineno == '':
                    newlineno = lineno
                else:
				    newlineno = newlineno + " " + lineno
                current_line = current_line.replace('\n','')
                current_line += value
            elif typ == "SEMICOLON":
                current_line = current_line.replace('\n','')
                current_line += value + '\n'
                data.append(lineno+ '\n')
                data.append(current_line)
                newlineno = ''
                current_line = ''
            elif (typ == "LBRACE"):
                current_line = current_line.replace('\n','')
                if current_line != '' and (current_line[len(current_line) - 1] != '\n'):
                    data.append(newlineno + '\n')
                    newline = current_line + '\n' + lineno + '\n' + value + '\n'
                else:
                    data.append(newlineno)
                    newline = lineno + '\n' + value + '\n'
                data.append(newline)
                newlineno = ''
                current_line = ''
            elif (typ == "RBRACE"):
                current_line = current_line.replace('\n','')
                data.append(lineno + '\n')
                
                if (current_line != "") and (current_line[len(current_line) - 1] != '\n'):
                  dataline1 = current_line + '\n' 
                  dataline2 = value + '\n'
                  data.append(dataline1)
                  data.append(lineno + '\n')
                  data.append(dataline2)
                else:
                  dataline = current_line + value + '\n'
                  data.append(dataline)
                newlineno = ''
                current_line = ''
                
            elif (typ == "FOR"):
                current_line += value
                current_line = current_line.replace('\n','')
                data.append(lineno + '\n')
                data.append(current_line + '\n')
                newlineno = ''
                current_line = ''
            elif (typ == "PACKAGE") or (typ == "IMPORT"):
                current_line += value
                data.append(lineno + '\n')
                data.append(current_line + '\n')
                newlineno = ''
                current_line = ''
            elif (typ == "COMMENT"):
                if commentflag == 0:
                    commentflag += 1
                    if self.printoption == 'True':
                        print "Warning: Comments should be ignored, change t_COMMENT to t_ignore_COMMENT"
            else:  
                pass
                #raise Exception("Given unhandled token type: " + str(typ))


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
        "NEWLINE",
        "RBRACE",
        "WORD",
        "FOR",
        "ARRAY",
        "DIVIDE",
        "STRING",
        "CHAR",
        "SEMICOLON",
        "WHITESPACE",
        "STARNEWLINE",
        "COMMENTNEWLINE"

        )
    states = (
        ('array','inclusive'),
        ('comment', 'exclusive')
        )
    t_ignore = "\t"#used to be " \t" but this removed too many spaces that I wanted.

    #FIXME do multiline comments with lexer states
    def t_COMMENT(t):
        r'//[^\n]*'
        #return t
        pass
    def t_NEWLINE(t):
        r'\n'
        t.lexer.lineno += 1#len(t.value)
        #return t
        pass
    def t_STRING(t):
        r'\"(\\\\\\\")?(\\\"|[^\"\n])*?(\\\\)?\"'
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
    def t_begin_array(t):
        r'\[\][\n\r\t ]*\{'
        length = len(t.value)
        t.lexer.level += 1
        if t.lexer.level == 1:
            t.lexer.array_start = t.lexer.lexpos-length
        
        t.type = "ARRAY"
        t.lexer.push_state('array')
        pass
    def t_array_error(t):
        print "Unable to parse value: "+ str(t.value[0])
        t.lexer.skip(1)
        
    def t_array_WHITESPACE(t):
        r'(\n|\r|\t|[ ])+'
        pass
        
    def t_array_STRING(t):
        r'\"(\\\\\\\")?(\\\"|[^\"])*?(\\\\)?\"'
        pass
    def t_array_CHAR(t):
        r'\'(\\[^\\]|\\\'|[^\\]|\\\\|\\[0-9A-Za-z]+)\''
        pass
    def t_array_WORD(t):
        r'([^\"\'\{\};/])+|\[\][ \t\n\r]*[^\{]'
        pass
    t_array_SEMICOLON = ";"
    def t_array_end(t):
        r'\}'
        t.lexer.pop_state()
        t.lexer.level -= 1
        t.value = t.lexer.lexdata[t.lexer.array_start:t.lexer.lexpos]
        t.type = "ARRAY"
        if t.lexer.level == 0:
           return t
        else:
            0
    def t_begin_comment(t):
        #r'/[*][^\n]*?(\n|\r\n)'
        #r'/[*][^\n]*?(\n|\r\n)?'
        r'/[*]'
        length = len(t.value)
        t.lexer.comment_start = t.lexer.lexpos-length
        t.type = "COMMENT"
        t.lexer.begin('comment')
    def t_comment_COMMENTNEWLINE(t):
        r'\n'
        t.lexer.lineno += 1#len(t.value)
        #return t
        pass    
    def t_comment_end(t):
        r'[*]/'
        t.value = t.lexer.lexdata[t.lexer.comment_start:t.lexer.lexpos]
        t.type = "COMMENT"
        t.lexer.begin('INITIAL')  
    def t_comment_STARNEWLINE(t):
        r'[*]\n|[*]\r\n'
        t.lexer.lineno += 1#len(t.value)
        #return t
        pass        
    def t_comment_COMMENT(t):
        #r'([^*/\n][*]*|[^/*\n]*[*])+'
        r'([^*\n]|[*][^/\n])+' #Anything is fine as long as it isnt a star,
                                 #If it is a star, a slash cannot follow it.
        #return t
        pass
    def t_comment_error(t):
        length = len(t.value)
        if len(t.value) > 200:
            string = t.value[0:90] + " ... " + t.value[length-90:length-1]
        else:
            string = t.value
        raise Exception("Error in comment, unable to tokenize value: " + string)
    t_comment_ignore = r''


        
    def t_LBRACE(t):
        r'\{'
        return t
    def t_RBRACE(t):
        r'\}'
        return t
    def t_FOR(t):
        r'for[ \t]*?\([^;\{]*;[^;\{]*;[^;\{]*?\)|for[ \t]*?\([^;\{]+:[^;\{]\)|else[ ]+for[ ]*?\([^;\{]*;[^;\{]*;[^;\{]*?\)|else[ ]+for[ ]*?\([^;\{]+:[^;\{]\)'
        return t
    def t_DIVIDE(t):
        r'/'
        return t

    def t_WORD(t):
        #r'([^\"\'\{\};/\[\]\n\t])+'
        r'([^\"\'\{\};/\[\]\n])+|\[.*?\][ \t\r]*?[^\{\n]'
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
    lexer.level = 0
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
    parsed = OneAtomPerLine(tokenized,filepath, ext, printopt)


main()


