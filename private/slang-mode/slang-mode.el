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
   '(("\\<\\(AppendStructuredBuffer\\|asm\\|asm_fragment\\|BlendState\\|break\\|Buffer\\|ByteAddressBuffer\\|case\\|cbuffer\\|centroid\\|class\\|column_major\\|compile\\|compile_fragment\\|CompileShader\\|const\\|continue\\|ComputeShader\\|ConsumeStructuredBuffer\\|default\\|DepthStencilState\\|DepthStencilView\\|discard\\|do\\|DomainShader\\|else\\|export\\|extern\\|for\\|fxgroup\\|GeometryShader\\|groupshared\\|Hullshader\\|if\\|in\\|inline\\|inout\\|InputPatch\\|interface\\|line\\|lineadj\\|linear\\|LineStream\\|matrix\\|namespace\\|nointerpolation\\|noperspective\\|out\\|OutputPatch\\|packoffset\\|pass\\|pixelfragment\\|PixelShader\\|point\\|PointStream\\|precise\\|RasterizerState\\|RenderTargetView\\|return\\|register\\|row_major\\|RWBuffer\\|RWByteAddressBuffer\\|RWStructuredBuffer\\|RWTexture1D\\|RWTexture1DArray\\|RWTexture2D\\|RWTexture2DArray\\|RWTexture3D\\|sample\\|sampler\\|SamplerState\\|SamplerComparisonState\\|shared\\|stateblock\\|stateblock_state\\|static\\|string\\|struct\\|switch\\|StructuredBuffer\\|tbuffer\\|technique\\|technique10\\|technique11\\|texture\\|Texture1D\\|Texture1DArray\\|Texture2D\\|Texture2DArray\\|Texture2DMS\\|Texture2DMSArray\\|Texture3D\\|TextureCube\\|TextureCubeArray\\|typedef\\|triangle\\|triangleadj\\|TriangleStream\\|uniform\\|vector\\|vertexfragment\\|VertexShader\\|void\\|volatile\\|while\\|domain\\|earlydepthstencil\\|instance\\|maxtessfactor\\|numthreads\\|outputcontrolpoints\\|outputtopology\\|partitioning\\|patchconstantfunc\\|SV_DispatchThreadID\\|SV_DomainLocation\\|SV_TessFactor\\|SV_GroupID\\|SV_GroupIndex\\|SV_GroupThreadID\\|SV_GSInstanceID\\|SV_InsideTessFactor\\|SV_OutputControlPointID\\)" . font-lock-keyword-face)
     ("\\<\\(bool\\|int\\|uint\\|half\\|float\\|double\\|int16_t\\|int32_t\\|int64_t\\|uint16_t\\|uint32_t\\|uint64_t\\|float16_t\\|float32_t\\|float64_t\\)\\([1234]\\|[1234]x[1234]\\)?\\>" . font-lock-type-face)
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
