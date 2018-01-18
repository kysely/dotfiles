filetype plugin on
syntax enable

set hidden
set number
set path+=**
set wildmenu
set splitright
set nocompatible
set laststatus=2
set showtabline=2
execute pathogen#infect()

" INDENTATION:
set smartindent
set tabstop=4 shiftwidth=4 noexpandtab nosmarttab autoindent
autocmd FileType python setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType cython setlocal tabstop=4 shiftwidth=4 noexpandtab

" VERTICAL RULERS:
set cc=90

" COLOR CUSTOMIZATION:
hi ColorColumn ctermbg=Black guibg=Black
hi VertSplit ctermfg=Black ctermbg=Black

" COLOR SCHEME:
set termguicolors
let ayucolor="mirage"
colorscheme ayu

" KEY SHORTCUTS:
imap jk <Esc>
imap JK <Esc>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap sidebar 30<C-w><Bar>
nnoremap sideterm 53<C-w><Bar>

nnoremap ter :ConqueTerm bash<CR>
nnoremap vter :ConqueTermVSplit bash<CR><Esc>53<C-w><Bar>i
nnoremap tt <C-w>li
nnoremap vex :Vexplore .<CR>30<C-w><Bar><C-w>l
nnoremap vsp :vsplit 
nnoremap sp :split 
nnoremap gt :bn<CR>
nnoremap gn :bp<CR>
nnoremap tabs :ls<CR>

" SNIPPETS:
imap ,doc """<Esc>o"""<Esc>ka
imap ,class <Esc>:-1read $HOME/.vim/snippets/class_template.py<CR>wce

" COMPLETOR:
let g:completor_clang_binary = '/usr/bin/clang'
let g:completor_python_binary = '/Users/radek/anaconda3/bin/python3'
let g:completor_node_binary = '/usr/local/bin/node'
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"

" NETRW Tree View:
let g:netrw_banner=0
let g:netrw_winsize=20
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" LIGHTLINE:
let g:lightline = {
    \ 'colorscheme': 'Dracula',
    \ 'active': {
    \   'right': [ [ 'lineinfo' ],
    \              [ 'fileformat', 'fileencoding', 'filetype' ] ]
    \ },
    \ 'tabline': {
    \   'left': [ [ 'bufferinfo' ],
    \             [ 'separator' ],
    \             [ 'bufferbefore', 'buffercurrent', 'bufferafter' ], ],
    \   'right': [ ],
    \ },
    \ 'component_expand': {
    \   'buffercurrent': 'lightline#buffer#buffercurrent',
    \   'bufferbefore': 'lightline#buffer#bufferbefore',
    \   'bufferafter': 'lightline#buffer#bufferafter',
    \ },
    \ 'component_type': {
    \   'buffercurrent': 'tabsel',
    \   'bufferbefore': 'raw',
    \   'bufferafter': 'raw',
    \ },
    \ 'component_function': {
    \   'bufferinfo': 'lightline#buffer#bufferinfo',
    \ },
    \ 'component': {
    \   'separator': '',
    \ },
    \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
    \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" }
    \ }


let g:lightline_buffer_logo = ''
let g:lightline_buffer_readonly_icon = ''
"let g:lightline_buffer_modified_icon = '✭'
let g:lightline_buffer_modified_icon = '+'
let g:lightline_buffer_git_icon = ' '
let g:lightline_buffer_ellipsis_icon = '..'
let g:lightline_buffer_expand_left_icon = '◀ '
let g:lightline_buffer_expand_right_icon = ' ▶'
let g:lightline_buffer_active_buffer_left_icon = ''
let g:lightline_buffer_active_buffer_right_icon = ''
let g:lightline_buffer_separator_icon = '  '

let g:lightline_buffer_show_bufnr = 1
let g:lightline_buffer_rotate = 0
let g:lightline_buffer_fname_mod = ':t'
let g:lightline_buffer_excludes = ['vimfiler']

let g:lightline_buffer_maxflen = 30
let g:lightline_buffer_maxfextlen = 3
let g:lightline_buffer_minflen = 16
let g:lightline_buffer_minfextlen = 3
let g:lightline_buffer_reservelen = 20

" CTRLP:
let g:ctrlp_show_hidden = 1

