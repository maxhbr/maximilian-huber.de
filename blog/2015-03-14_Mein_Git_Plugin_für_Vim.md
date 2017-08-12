Der Plugin *myVersionControl* bietet minimale Git und SVN unterstützung für Vim.
Er enthält beisplielsweise die Funktionen

* *:GPush* (Führt ein `git push` aus)

* *:GCommit* (Fragt nach einer Nachricht und führt danach ein `git commit -a -m
  Nachricht` aus)

* *:GAdd* (Fügt die aktuelle Datei zu Git hinzu)

* *:GAuto* (macht ein GCommit und direkt danach ein GPush)

* und mehr

Die autoload-Datei
[.vim/autoload/myVersionControl.vim](https://github.com/maxhbr/myconfig/blob/master/vim/autoload/myVersionControl.vim):
```Vim Script
" minimal git/svn wrapper written by maximilian-huber.de
" ====  Git  ========================================================
function! myVersionControl#GitCommit()
  let msg = 0 < a:0 ? a:1 : inputdialog("Msg: ")
  execute '!git commit -a -m "' msg '"'
endfunction

function! myVersionControl#GitPush()
  execute '!git push'
endfunction

function! myVersionControl#GitPull()
  execute '!git pull'
endfunction

function! myVersionControl#GitAdd()
  execute '!git add %'
endfunction

function! myVersionControl#GitStatus()
  execute '!git status'
endfunction

function! myVersionControl#GitCheckout()
  let msg = 0 < a:0 ? a:1 : inputdialog("Branch: ")
  execute '!git checkout ' msg
endfunction

function! myVersionControl#GitBranch()
  let msg = 0 < a:0 ? a:1 : inputdialog("Create Branch: ")
  execute '!git checkout -b ' msg
endfunction

function! myVersionControl#GitAuto()
  call myVersionControl#GitCommit()
  call myVersionControl#GitPush()
endfunction

"
" ====  SVN  ========================================================
function! myVersionControl#SVNCommit()
  let msg = 0 < a:0 ? a:1 : inputdialog("Msg: ")
  execute '!svn commit -m "' msg '"'
endfunction

function! myVersionControl#SVNUpdate()
  execute '!svn update'
endfunction

function! myVersionControl#SVNAdd()
  execute '!svn add %'
endfunction
```
Die Datei
[.vim/plugin/myVersionControl.vim](https://github.com/maxhbr/myconfig/blob/master/vim/plugin/myVersionControl.vim):
```Vim Script
" minimal git/svn wrapper written by maximilian-huber.de
" ====  Git  ========================================================
command! GCommit   call myVersionControl#GitCommit()
command! GPush     call myVersionControl#GitPush()
command! GPull     call myVersionControl#GitPull()
command! GAdd      call myVersionControl#GitAdd()
command! GStatus   call myVersionControl#GitStatus()
command! GCheckout call myVersionControl#GitCheckout()
command! GBranch   call myVersionControl#GitBranch()
command! GAuto     call myVersionControl#GitAuto()

nnoremap <silent> _gc :call myVersionControl#GitCommit()<cr>

" ====  SVN  ========================================================
command! SVNCommit call myVersionControl#SVNCommit()
command! SVNUpdate call myVersionControl#SVNUpdate()
command! SVNAdd    call myVersionControl#SVNAdd()
```
