
" populate arglist:
:args `git grep -l <string>`

" run command on all files on arglist:
:argdo %s/<string>/<replacement>/gce | update

" flags:
"   g[lobal]: replace all matches
"   c[onfirm]: ask for confirmation before replacing
"   e[rror]: don’t halt operation at errors

" `update` saves each file after the replacement is made.
