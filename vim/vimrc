filetype plugin on
syntax enable
colorscheme base16-tomorrow-night

set nocompatible
set path+=**
set wildmenu
set number
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
set cc=90
hi ColorColumn ctermbg=lightgrey guibg=lightgrey

" KEY SHORTCUTS:
imap jk <Esc>
nnoremap <C-d> :sh<CR>

" NETRW Tree View:
let g:netrw_banner=0
let g:netrw_winsize=20
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" TAG JUMPING:
command! MakeTags !ctags -R .

" SNIPPETS:
nnoremap ,class :-1read $HOME/.vim/.class_template.py<CR>

