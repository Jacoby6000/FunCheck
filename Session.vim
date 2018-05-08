let SessionLoad = 1
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/projects/FunCheck
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +11 parse-regex/src/FunCheck/Data/Regex.hs
badd +28 parse-regex/src/FunCheck/Parse/Regex.hs
badd +43 core/src/FunCheck/Generate.hs
badd +3 libs/yaya/README.md
badd +24 cli/src/Main.hs
badd +2 Setup.hs
badd +17 core/src/FunCheck/Data/Template.hs
badd +5 libs/yaya/hedgehog/src/Yaya/Hedgehog.hs
badd +2 libs/yaya/hedgehog/src/Yaya/Hedgehog/Expr.hs
badd +2 cli/funcheck-cli.cabal
badd +0 ~/projects/dotfiles/vim/ftplugin/haskell.vim
argglobal
silent! argdel *
edit parse-regex/src/FunCheck/Data/Regex.hs
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd _ | wincmd |
split
1wincmd k
wincmd w
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winminheight=1 winminwidth=1 winheight=1 winwidth=1
exe '1resize ' . ((&lines * 27 + 29) / 58)
exe 'vert 1resize ' . ((&columns * 49 + 79) / 159)
exe '2resize ' . ((&lines * 27 + 29) / 58)
exe 'vert 2resize ' . ((&columns * 49 + 79) / 159)
exe 'vert 3resize ' . ((&columns * 109 + 79) / 159)
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=20
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 12 - ((6 * winheight(0) + 13) / 27)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
12
normal! 0
lcd ~/projects/FunCheck
wincmd w
argglobal
if bufexists('~/projects/dotfiles/vim/ftplugin/haskell.vim') | buffer ~/projects/dotfiles/vim/ftplugin/haskell.vim | else | edit ~/projects/dotfiles/vim/ftplugin/haskell.vim | endif
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=20
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 18 - ((10 * winheight(0) + 13) / 27)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
18
normal! 0
lcd ~/projects/FunCheck
wincmd w
argglobal
if bufexists('~/projects/FunCheck/parse-regex/src/FunCheck/Parse/Regex.hs') | buffer ~/projects/FunCheck/parse-regex/src/FunCheck/Parse/Regex.hs | else | edit ~/projects/FunCheck/parse-regex/src/FunCheck/Parse/Regex.hs | endif
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=20
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 26 - ((25 * winheight(0) + 27) / 55)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
26
normal! 022|
lcd ~/projects/FunCheck
wincmd w
3wincmd w
exe '1resize ' . ((&lines * 27 + 29) / 58)
exe 'vert 1resize ' . ((&columns * 49 + 79) / 159)
exe '2resize ' . ((&lines * 27 + 29) / 58)
exe 'vert 2resize ' . ((&columns * 49 + 79) / 159)
exe 'vert 3resize ' . ((&columns * 109 + 79) / 159)
tabnext 1
if exists('s:wipebuf') && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 winminheight=1 winminwidth=1 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
