
The lexer is based on
https://timsong-cpp.github.io/cppwp/n4140/lex.pptoken

preprocessing-token:
    header-name
    identifier
    pp-number
    character-literal
    user-defined-character-literal
    string-literal
    user-defined-string-literal
    preprocessing-op-or-punc
    each non-white-space character that cannot be one of the above

----------------
(Only in the #include directive)
header-name:
    < h-char-sequence >
    " q-char-sequence "

h-char-sequence:
    h-char
    h-char-sequence h-char

h-char:
    any member of the source character set except new-line and >

q-char-sequence:
    q-char
    q-char-sequence q-char

q-char:
    any member of the source character set except new-line and "

----------------
identifier:
    identifier-nondigit
    identifier identifier-nondigit
    identifier digit

identifier-nondigit:
    nondigit
    universal-character-name
    other implementation-defined characters

nondigit: one of
    a b c d e f g h i j k l m
    n o p q r s t u v w x y z
    A B C D E F G H I J K L M
    N O P Q R S T U V W X Y Z _

digit: one of
    0 1 2 3 4 5 6 7 8 9

------------------------

pp-number:
    digit
    . digit
    pp-number digit
    pp-number identifier-nondigit
    pp-number ' digit
    pp-number ' nondigit
    pp-number e sign
    pp-number E sign
    pp-number .

------------------------

character-literal:
    ' c-char-sequence '
    u' c-char-sequence '
    U' c-char-sequence '
    L' c-char-sequence '

c-char-sequence:
    c-char
    c-char-sequence c-char

c-char:
        any member of the source character set except
                the single-quote ', backslash \, or new-line character
        escape-sequence
        universal-character-name

escape-sequence:
    simple-escape-sequence
    octal-escape-sequence
    hexadecimal-escape-sequence

simple-escape-sequence: one of
    \'  \"  \?  \\
    \a  \b  \f  \n  \r  \t  \v

octal-escape-sequence:
    \ octal-digit
    \ octal-digit octal-digit
    \ octal-digit octal-digit octal-digit

hexadecimal-escape-sequence:
    \x hexadecimal-digit
    hexadecimal-escape-sequence hexadecimal-digit

-------------------------------

user-defined-character-literal:
    character-literal ud-suffix

ud-suffix:
    identifier

------------------------

string-literal:
    encoding-prefixopt " s-char-sequenceopt "
    encoding-prefixopt R raw-string

encoding-prefix:
  u8
  u
  U
  L

s-char-sequence:
    s-char
    s-char-sequence s-char

s-char:
        any member of the source character set except
                the double-quote ", backslash \, or new-line character
        escape-sequence
        universal-character-name

raw-string:
    " d-char-sequenceopt ( r-char-sequenceopt ) d-char-sequenceopt "

r-char-sequence:
    r-char
    r-char-sequence r-char

r-char:
        any member of the source character set, except
                a right parenthesis ) followed by the initial d-char-sequence
                (which may be empty) followed by a double quote ".

d-char-sequence:
    d-char
    d-char-sequence d-char

d-char:
        any member of the basic source character set except:
                space, the left parenthesis (, the right parenthesis ), the backslash \,
                and the control characters representing horizontal tab,
                vertical tab, form feed, and newline.

---------------------------------
preprocessing-op-or-punc: one of
        {       }       [       ]       #       ##      (       )
        <:      :>      <%      %>      %:      %:%:    ;       :       ...
        new     delete          ?       ::      .       .*
        +       -       *       /       %       ^       &       |       ~
        !       =       <       >       +=      -=      *=      /=      %=
        ^=      &=      |=      <<      >>      >>=     <<=     ==      !=
        <=      >=      &&      ||      ++      --      ,       ->*     ->
        and     and_eq          bitand          bitor   compl   not     not_eq
        or      or_eq   xor     xor_eq

-------------------

user-defined-string-literal:
    string-literal ud-suffix
