"
" delete lines 3-18:
:3,18d
" delete line 3 into reg x:
:3d x
" moves/copy lines 3-5 below current line:
:3,5m.
:3,5t.
" executes normal mode command on range (in this case, .):
:'<,'>norm .
" total # of lines:
:=
" deletes from current line to end
:.,$d
" deletes whole file
:%d
" deletes from current to current +5 lines
:.,+5d
" moves from line 25 until end of file to 2 lines above current line
:25,$m-2

" delete next line containing pattern (d can be replaced with any ex cmd):
:/pattern/d
" delete all lines containing pattern:
:g/pattern/d
" delete all lines NOT containing pattern:
:v/pattern/d

" read file in current buffer
:r file
