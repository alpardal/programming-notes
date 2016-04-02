
" escape to use with regex meaning: +, (, ), <, >
" examples:
    " matches a plus sign, inside brackets, followed by <>:
    /(+)<>
    " matches and groups one or more \w between word boundaries:
    /\(\<\w\+\>\)

" POSIX character classes (must always be used inside []):
" [:alnum:], [:alpha:], [:blank:], [:lower:], [:upper:],
" [:punct:], [:space:], [:digit:], [:xdigit:]
    " search capitalized strings:
    /[[:upper:]][[:lower]]\+
