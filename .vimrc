syntax enable
filetype plugin on

set nocompatible
set path+=**
set wildmenu
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

command! MakeTags !ctags -R .

let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" KEY SHORTCUTS:
imap jk <Esc>
nnoremap <C-d> :sh<CR>

" SNIPPETS:
nnoremap ,class :-1read $HOME/.vim/.class_template.py<CR>

