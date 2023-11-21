(progn
  (require 'package)
  (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (package-initialize)
  (package-refresh-contents))
