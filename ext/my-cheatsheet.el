;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Cheat sheet https://github.com/darksmile/cheatsheet

;;; Code:

(require 'cheatsheet)

(global-set-key [f1] 'cheatsheet-show)

(cheatsheet-add-group 'File
		      '(:key "C-x C-f" :description "Open file")
		      '(:key "C-c k"   :description "Ag project aware"))

(cheatsheet-add-group 'Text
		      '(:key "C-x u/v" :description "Visual undo")
		      '(:key "C-w" :description "Kill backward word")
		      '(:key "M-w" :description "Copy to kill ring")
		      '(:key "M-y" :description "Show kill ring")
		      '(:key "C-/" :description "Undo")
		      '(:key "C-S-backspace" :description "Kill whole line")
		      '(:key "C-k" :description "Kill rest of line")
		      '(:key "C-c C-o" :description "Follow link"))

(cheatsheet-add-group 'Project
		      '(:key "C-c p f" :description "Project File")
		      '(:key "C-c p s s" :description "Project search")
		      '(:key "C-c p p" :description "Switch project")
		      '(:key "C-c p b" :description "Switch buffer")
		      '(:key "C-x m"   :description "Maggit Status"))

(cheatsheet-add-group 'Org
		      '(:key "S-<arrow-down/up>" :description "Move item down or up"))


(cheatsheet-add-group 'Help
		      '(:key "<f2> f/v/l/i" :description "describe function, variable, library, symbol"))

(cheatsheet-add-group 'Java
		      '(:key "C-c C-j j" :description "javadoc lookup")
		      '(:key "C-c C-j i" :description "import all")
		      '(:key "C-c C-j o" :description "optimize imports")
		      '(:key "C-c C-j l" :description "local variable")
		      '(:key "C-c C-j c f" :description "compile file")
		      '(:key "C-c C-j c p" :description "compile project")
		      '(:key "C-c C-j t c" :description "junit class")
		      '(:key "C-c C-j t t" :description "junit test case")
		      '(:key "M-."       :description "jump to declaration")
		      '(:key "M-,"       :description "jump back")
		      '(:key "M-x company-complete" :description "Find class then use M-. to jump to symbol")
		      '(:key "<tab>"  :description "Indent or if a yasnippet key pa (package) con(constructor), p (print) cla (class) int(interface) prop(get/set) new list set tos main ife tryc"))

(provide 'my-cheatsheet)
;;; my-cheatsheet.el ends here
