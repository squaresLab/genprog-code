This is a "regression test" for the stringrep representation. It's just
like the classic GCD repair, but instead of using a CIL AST backbone,
it treats the file as an uninterpreted list of strings (one per line).
It does no fault localization. Despite this, it should fine a repair within
60 or so variants. 

(Note that the number of lines has been compressed to make this easier.) 

See "./configuration" for details. 

weimer - Sat Jul 24 16:08:03 EDT 2010
