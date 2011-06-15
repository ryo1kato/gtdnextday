" Vim syntax file
" Language: gtd journal
" Creator:  Ryoichi KATO <ryo1kato@gmail.com>
" Just a modified go.vim syntax file
"
" gtdj stands for "GTD Journal"
"
colorscheme blue

syn match gtdj_DATE           display /20[0-9][0-9]-[012][0-9]-[0-3][0-9]/
syn match gtdj_HEADER         display /^\([A-Z]\{3,\}.*\|#.*\|■.*\)$/
syn match gtdj_TAG            display /^@[A-Za-z_0-9]\{3,\}.*/
syn match gtdj_ITEM_DONE      display /\(<X>\|\[X\]\).*/
syn match gtdj_ITEM_DONEKEEP  display /\(<x>\|\[x\]\).*/
syn match gtdj_ITEM_DOINGNOW  display /\(<o>\|\[o\]\).*/                   contains=@gtdj_emph
syn match gtdj_ITEM_CANCEL    display /\(<->\|\[-\]\).*/
syn match gtdj_ITEM_DOING     display /\(<^[Xxo -]>\|\[[^Xxo -]\]\).*/     contains=@gtdj_emph
syn match gtdj_ITEM           display /\(< >\|\[ \]\).*/                   contains=@gtdj_emph
syn match gtdj_NOTE           display /^\s*\* .*$/                         contains=@gtdj_emph
syn match gtdj_QUOTE          display /^\s*> /                             contains=@gtdj_emph
syn match gtdj_COMMANDS       display /^\s*| /                             contains=@gtdj_emph
syn match gtdj_LINE           display /^-\{8,\}/
syn match gtdj_DOUBLE_LINE    display /^=\{8,\}/
syn match gtdj_LINE           display /^[\(\- \)]*-/

syntax cluster gtdj_emph contains=gtdj_BoldUnderline,gtdj_Bold
syn match gtdj_BoldUnderline  display /_.*_/
syn match gtdj_Bold           display /\*[^ ].*\*/

syntax region gtdj_Comment start="/\*" end="\*/"

hi link gtdj_DATE           SpecialChar
hi link gtdj_NEXT           Operator
hi link gtdj_HEADER         Operator
hi link gtdj_TAG            SpecialChar
hi link gtdj_ITEM_DONE      Comment
hi link gtdj_ITEM_DONEKEEP  Comment
"hi link gtdj_ITEM_DOINGNOW  Error
hi gtdj_ITEM_DOINGNOW  term=bold,reverse cterm=bold,reverse gui=bold,reverse
hi link gtdj_ITEM_CANCEL    cIf0
hi link gtdj_ITEM_DOING     Operator
hi link gtdj_ITEM           Macro
hi link gtdj_NOTE           Character
hi link gtdj_QUOTE          SpecialChar
hi link gtdj_COMMANDS       Macro
hi link gtdj_LINE           Comment
hi link gtdj_DOUBLE_LINE    Macro

hi link gtdj_Comment        Comment

hi gtdj_BoldUnderline term=bold,underline cterm=bold,underline gui=bold,underline
hi gtdj_Bold          term=bold cterm=bold gui=bold
hi gtdj_Underline     term=underline cterm=underline gui=underline
hi gtdj_Italic        term=italic cterm=italic gui=italic


"Bold                term=bold cterm=bold gui=bold
"BoldUnderline       term=bold,underline cterm=bold,underline gui=bold,underline
"BoldItalic          term=bold,italic cterm=bold,italic gui=bold,italic
"BoldUnderlineItalic term=bold,italic,underline cterm=bold,italic,underline gui=bold,italic,underline
"Underline           term=underline cterm=underline gui=underline
"UnderlineItalic     term=italic,underline cterm=italic,underline gui=italic,underline
"Italic              term=italic cterm=italic gui=italic


"syn keyword Label          Label
"syn keyword Label          Label
"syn keyword Conditional    Conditional
"syn keyword Repeat         Repeat
"syn keyword Character      Character
"syn keyword Number         Number
"syn keyword Number         Number
"syn keyword Float          Float
"syn keyword Operator       Operator
"syn keyword Structure      Structure
"syn keyword StorageClass   StorageClass
"syn keyword Include        Include
"syn keyword PreProc        PreProc
"syn keyword Macro          Macro
"syn keyword Error          Error
"syn keyword Statement      Statement
"syn keyword PreCondit      PreCondit
"syn keyword Type           Type
"syn keyword Constant       Constant
"syn keyword String         String
"syn keyword Comment        Comment
"syn keyword SpecialChar    SpecialChar


