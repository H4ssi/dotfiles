
set guioptions-=m

colorscheme Tomorrow-Night-Eighties

" fix issues with rainbow parents
au BufEnter * :syntax sync fromstart
au BufEnter * RainbowParentheses
let g:rainbow#max_level = 64

