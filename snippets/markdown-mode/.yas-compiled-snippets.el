;;; Compiled snippets and support files for `markdown-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'markdown-mode
                     '(("more" "<!--more-->\n" "more" nil nil nil "/Users/sulrich/.emacs.d/snippets/markdown-mode/more" nil nil)
                       ("mmtg" "# `(format-time-string \"%Y%m%d\")` - $0 \n\n### attendees / logistics\n\n- XXX:\n- arista:\n- location: [in-person mtg | hangout]\n\n## discussion notes\n\n\n## action items / next steps\n\n- [ ] \n" "mmtg" nil nil nil "/Users/sulrich/.emacs.d/snippets/markdown-mode/mmtg" nil nil)
                       ("mcomment" "[//]: #\n" "markdown comment" nil nil nil "/Users/sulrich/.emacs.d/snippets/markdown-mode/mcomment" nil nil)
                       ("frontmatter" "---\nlayout: post\npublished: false\nauthor: sulrich\ncategories: [nerd]\ndate: `(format-time-string \"%Y-%m-%d %H:%M:%S %z\")`\ntags: \n- writing\n- blogging\n- productivity \ntitle: \"%%TITLE%%\"\n---\n\n\n\n" "front-matter" nil nil nil "/Users/sulrich/.emacs.d/snippets/markdown-mode/frontmatter" nil nil)
                       ("anet-case" "[SR/$1](https://tac.aristanetworks.com/case/cv/view/$1)\n" "case-link" nil nil nil "/Users/sulrich/.emacs.d/snippets/markdown-mode/anet-case" nil nil)
                       ("anet-bug" "[BUG/$1](https://bugs.aristanetworks.com/$1)\n" "pr-link" nil nil nil "/Users/sulrich/.emacs.d/snippets/markdown-mode/anet-bug" nil nil)
                       ("anet-att" "    - karthik\n    - akhilesh\n    - lleavitt\n    - sulrich\n    - vchalasani\n" "arista attendees" nil nil nil "/Users/sulrich/.emacs.d/snippets/markdown-mode/anet-att" nil nil)))


;;; Do not edit! File generated at Tue Nov 24 11:02:33 2020
