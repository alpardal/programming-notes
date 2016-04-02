
map <leader>c :w\|!rm -rf bin && mkdir -p bin && erlc -o bin %<cr>
