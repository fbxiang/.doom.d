;;; slang-mode.el --- Major mode for Slang  -*- lexical-binding: t; -*-
;;; Commentary:

;; Major mode for editing Slang shader files

;;; Code:

(require 'cc-mode)
(require 'lsp-mode)

(defgroup slang nil
  "Slang major mode."
  :group 'langauges)

;;;###autoload
(define-derived-mode slang-mode c-mode "Slang"
  "Major mode for editing Slang shader files."
  (font-lock-add-keywords
   nil
   '(("\\<\\(cbuffer\\|numthreads\\|SV_GroupThreadID\\|SV_DispatchThreadID\\)" . font-lock-keyword-face)
     ("\\<\\(uint2\\|uint3\\|uint4\\|int2\\|int3\\|int4\\|float2\\|float3\\|float4\\)" . font-lock-type-face)
     ("\\<\\w+\\s-+\\(\\w+\\)\\s-*:" 1 font-lock-variable-name-face t)
     )
   'append))

;;;###autoload
(progn (add-to-list 'auto-mode-alist '("\\.slang\\'" . slang-mode))
       (with-eval-after-load 'lsp-mode
         (add-to-list 'lsp-language-id-configuration '(slang-mode . "slang"))
         (lsp-register-client (make-lsp-client
                               :new-connection (lsp-stdio-connection "slangd")
                               :activation-fn (lsp-activate-on "slang")
                               :server-id 'slangd)))
       (add-hook 'slang-mode-hook (lambda () (lsp) (add-hook 'after-revert-hook #'lsp))))

(provide 'slang-mode)
;;; slang-mode.el ends here
