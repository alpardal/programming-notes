
" delimiters (any char except \, | and "):
:s/bla/boing
:s;some/path;another/path;g

" \1, \2, ... -> replace matched groups
:%s/\(this\) \(that\)/\2 \1/

" & -> entire match
:%s/John/&son

" ~ -> replace by last used replacement:
:%s/bla/something  " replaces bla with something
:%s/another/~      " replaces another with something

" \u, \l -> upper/lower case next char
" \U, \L -> upper/lower case until \e or \E
:%s/\(\w\)\(\w\+\)/\u\1\L\2/  " capitalizes every word

:& " -> repeats last substitution (or just &)
:%&g " -> repeats last subs globally
:~  " -> repeats last subs with last used search regex. e.g.:
    :%s/red/blue/g
    /green
    :%~g   " same as :%s/green/blue/g
