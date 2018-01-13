filetype plugin on
syntax enable

set nocompatible
set hidden
set path+=**
set wildmenu
set number

" INDENTATION:
set smartindent
set tabstop=4 shiftwidth=4 noexpandtab nosmarttab autoindent
autocmd FileType python setlocal tabstop=4 shiftwidth=4 noexpandtab

" VERTICAL RULERS:
set cc=90
hi ColorColumn ctermbg=Black guibg=Black

" COLOR CUSTOMIZATION:
hi TabLineFill ctermfg=Black ctermbg=NONE
hi TabLine ctermfg=Blue ctermbg=Black
hi TabLineSel ctermfg=Red ctermbg=NONE

hi VertSplit ctermfg=Black ctermbg=Black
hi StatusLine ctermfg=Black ctermbg=Black
hi StatusLineNC ctermfg=Black ctermbg=Black

" KEY SHORTCUTS:
imap jk <Esc>
imap JK <Esc>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <C-d> :sh<CR>
nnoremap vex :Vexplore .<CR>30<C-w><Bar><C-w>l
nnoremap vsp :vsplit 
nnoremap sp :split 
nnoremap gt :bN<CR>
nnoremap tabs :ls<CR>

nnoremap side 30<C-w><Bar>

" NETRW Tree View:
let g:netrw_banner=0
let g:netrw_winsize=20
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" SNIPPETS:
imap ,doc """<Esc>o"""<Esc>ka
imap ,class <Esc>:-1read $HOME/.vim/snippets/class_template.py<CR>wce

