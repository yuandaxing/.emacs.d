(require 'cl-lib)
(require 'company)
(require 'company-template)
(require 'dash)
(require 'json)
(require 'url)
(use-package request
  :ensure t
  :config
  (progn
    (require 'request)))

(defconst ai-complete-url "http://gpu4ss.jx.shbt.qihoo.net:60010/code-complete")
(defconst 360-aicomplete--buffer-name "*360-aicomplete-log*")
(defconst 360-aicomplete--hooks-alist nil)
(defconst 360-aicomplete--protocol-version "1.0.14")

;; tmp file put in 360-aicomplete-binaries-folder directory
(defconst 360-aicomplete--version-tempfile "version")

;; current don't know how to use Prefetch and GetIdentifierRegex
(defconst 360-aicomplete--method-autocomplete "Autocomplete")
(defconst 360-aicomplete--method-prefetch "Prefetch")
(defconst 360-aicomplete--method-getidentifierregex "GetIdentifierRegex")
(defmacro 360-aicomplete-with-disabled (&rest body)
  "Run BODY with `360-aicomplete' temporarily disabled.
Useful when binding keys to temporarily query other completion backends."
  `(let ((360-aicomplete--disabled t))
     ,@body))
(defun 360-aicomplete--filename-completer-p (extra-info)
  "Check whether candidate's EXTRA-INFO indicates a filename completion."
  (-contains? '("[File]" "[Dir]" "[File&Dir]") extra-info))

(defun 360-aicomplete--identifier-completer-p (extra-info)
  "Check if candidate's EXTRA-INFO indicates a identifier completion."
  (s-equals? "[ID]" extra-info))

;;
;; Customization
;;

(defgroup 360-aicomplete nil
  "Options for 360-aicomplete."
  :link '(url-link :tag "Github" "https://github.com/TommyX12/360-aicomplete")
  :group 'company
  :prefix "360-aicomplete-")

(defcustom 360-aicomplete-max-num-results 10
  "Maximum number of results to show."
  :group '360-aicomplete
  :type 'integer)

(defcustom 360-aicomplete-context-radius 3000
  "The number of chars before point to send for completion.

Note that setting this too small will cause 360-Aicomplete to not be able to read the entire license activation key."
  :group '360-aicomplete
  :type 'integer)

(defcustom 360-aicomplete-context-radius-after 1000
  "The number of chars after point to send for completion."
  :group '360-aicomplete
  :type 'integer)

(defcustom 360-aicomplete-max-restart-count 10
  "Maximum number of times 360-Aicomplete can consecutively restart.
This may be due to errors in or automatic server updates.
Any successful completion will reset the consecutive count."
  :group '360-aicomplete
  :type 'integer)

(defcustom 360-aicomplete-wait 2.
  "Number of seconds to wait for 360-Aicomplete to respond."
  :group '360-aicomplete
  :type 'float)

(defcustom 360-aicomplete-always-trigger t
  "Whether to overload company's minimum prefix length.
This allows completion to trigger on as much as possible.
Default is t (strongly recommended)."
  :group '360-aicomplete
  :type 'boolean)

(defcustom 360-aicomplete-no-continue nil
  "Whether to make company reset idle timer on all keystrokes.
Only useful when `idle-delay' is not 0.
Doing so improves performance by reducing number of calls to the completer,
at the cost of less responsive completions."
  :group '360-aicomplete
  :type 'boolean)

(defcustom 360-aicomplete-log-file-path nil
  "If non-nil, next 360-Aicomplete restart will write debug log to this path."
  :group '360-aicomplete
  :type 'string)

(defcustom 360-aicomplete-auto-balance t
  "Whether 360-Aicomplete should insert balanced parentheses upon completion."
  :group '360-aicomplete
  :type 'boolean)

(defcustom 360-aicomplete-show-annotation t
  "Whether to show an annotation inline with the candidate."
  :group '360-aicomplete
  :type 'boolean)

(defcustom 360-aicomplete-auto-fallback t
  "Whether to automatically fallback to other backends when 360-Aicomplete has no candidates."
  :group '360-aicomplete
  :type 'boolean)

(defcustom 360-aicomplete-use-native-json t
  "Whether to use native JSON when possible."
  :group '360-aicomplete
  :type 'boolean)

(defcustom 360-aicomplete-insert-arguments t
  "When non-nil, insert function arguments as a template after completion.
Only supported by modes in `360-aicomplete--extended-features-modes'"
  :group '360-aicomplete
  :type 'boolean)

(defvar 360-aicomplete--response nil
  "Temporarily stored 360-Aicomplete server responses.")

(defvar 360-aicomplete--disabled nil
  "Variable to temporarily disable 360-aicomplete and pass control to next backend.")

(defvar 360-aicomplete--calling-continue nil
  "Flag for when `company-continue' is being called.")

(defvar 360-aicomplete--response-chunks nil
  "The string to store response chunks from 360-Aicomplete server.")

(defun 360-aicomplete--prefix-candidate-p (candidate prefix)
  "Return t if CANDIDATE string begins with PREFIX."
  (let ((insertion-text (cdr (assq 'insertion_text candidate))))
    (s-starts-with? prefix insertion-text t)))

(defun http-post-request (prev)
  (let ((result nil))
    (funcall 'request
      ai-complete-url
      :type "POST"
      :data (json-encode `(("text" . ,prev)))
      :headers '(("Content-type" . "application/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq 360-aicomplete--response (assoc-default 'complete data))))
          :sync t
          )
    ))
;(message "%s" (http-post-request "#include <iostream>\nint main"))

(defun 360-aicomplete-send-request (request)
  (http-post-request request)
  )

(defun 360-aicomplete--make-request ()
  "Create request body for method METHOD and parameters PARAMS."
  (let* ((buffer-min 1)
         (buffer-max (1+ (buffer-size)))
         (before-point
          (max (point-min) (- (point) 360-aicomplete-context-radius)))
         (after-point
          (min (point-max) (+ (point) 360-aicomplete-context-radius-after))))
    (buffer-substring-no-properties before-point (point))
    )
  )

(defun 360-aicomplete-query ()
  "Query 360-Aicomplete server for auto-complete."
  (let ((request (360-aicomplete--make-request)))
    (360-aicomplete-send-request request)
    ))

(defun 360-aicomplete--decode (msg)
  "Decode 360-Aicomplete server response MSG, and return the decoded object."
  (if (and 360-aicomplete-use-native-json
           (fboundp 'json-parse-string))
      (ignore-errors
        (json-parse-string msg :object-type 'alist))
    (let ((json-array-type 'list)
          (json-object-type 'alist))
      (json-read-from-string msg))))

(defun 360-aicomplete--process-sentinel (process event)
  "Sentinel for 360-Aicomplete server process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and 360-aicomplete--process
             (memq (process-status process) '(exit signal)))

    (message "360-Aicomplete process %s received event %s."
             (prin1-to-string process)
             (prin1-to-string event))

    (if (>= 360-aicomplete--restart-count
            360-aicomplete-max-restart-count)
        (progn
          (message "360-Aicomplete process restart limit reached.")
          (setq 360-aicomplete--process nil))

      (message "Restarting 360-Aicomplete process.")
      (360-aicomplete-start-process)
      (setq 360-aicomplete--restart-count
            (1+ 360-aicomplete--restart-count)))))

(defun 360-aicomplete--prefix ()
  "Prefix-command handler for the company backend."
  (if (or (and 360-aicomplete-no-continue
               360-aicomplete--calling-continue)
          360-aicomplete--disabled)
      nil
    (360-aicomplete-query)
    (let ((prefix
           (and 360-aicomplete--response
                (> (length 360-aicomplete--response) 0
                ))))
      (unless (or prefix
                  360-aicomplete-auto-fallback)
        (setq prefix 'stop))
      (if (and prefix
               360-aicomplete-always-trigger)
          ""
        prefix))))

(defun 360-aicomplete--annotation(candidate)
  "Fetch the annotation text-property from a CANDIDATE string."
  (when 360-aicomplete-show-annotation
    (-if-let (annotation (get-text-property 0 'annotation candidate))
        annotation
      (let ((kind (get-text-property 0 'kind candidate))
            ;; (return-type (get-text-property 0 'return_type candidate))
            (params (get-text-property 0 'params candidate)))
        (when kind
          (concat params
                  ;; (when (s-present? return-type)
                  ;;   (s-prepend " -> " return-type))
                  (when (s-present? kind)
                    (format " [%s]" kind))))))))

(defun 360-aicomplete--kind-to-type (kind)
  (pcase kind
    (1 "Text")
    (2 "Method")
    (3 "Function")
    (4 "Constructor")
    (5 "Field")
    (6 "Variable")
    (7 "Class")
    (8 "Interface")
    (9 "Module")
    (10 "Property" )
    (11 "Unit" )
    (12 "Value" )
    (13 "Enum")
    (14 "Keyword" )
    (15 "Snippet")
    (16 "Color")
    (17 "File")
    (18 "Reference")
    (19 "Folder")
    (20 "EnumMember")
    (21 "Constant")
    (22 "Struct")
    (23 "Event")
    (24 "Operator")
    (25 "TypeParameter")))

(defun 360-aicomplete--construct-candidate-generic (candidate)
  "Generic function to construct completion string from a CANDIDATE."
  (360-aicomplete--with-destructured-candidate candidate))

(defun 360-aicomplete--construct-candidates (results construct-candidate-fn)
  "Use CONSTRUCT-CANDIDATE-FN to construct a list of candidates from RESULTS."
  (let ((completions (mapcar construct-candidate-fn results)))
    (when completions
      (setq 360-aicomplete--restart-count 0))
    completions))

(defun 360-aicomplete--get-candidates (response)
  "Get candidates for RESPONSE."

  )

(defun 360-aicomplete--candidates (prefix)
  "Candidates-command handler for the company backend for PREFIX.

Return completion candidates.  Must be called after `360-aicomplete-query'."
  (mapcar (lambda (x) (aref x 0)) 360-aicomplete--response)
  )
(defun 360-aicomplete--meta (candidate)
  "Return meta information for CANDIDATE.  Currently used to display user messages."
  "360ai")

(defun 360-aicomplete--post-completion (candidate)
  "Replace old suffix with new suffix for CANDIDATE."
  (when 360-aicomplete-auto-balance
    (let ((old_suffix (get-text-property 0 'old_suffix candidate))
          (new_suffix (get-text-property 0 'new_suffix candidate)))
      (delete-region (point)
                     (min (+ (point) (length old_suffix))
                          (point-max)))
      (when (stringp new_suffix)
        (save-excursion
          (insert new_suffix))))))

;;
;; Interactive functions
;;

(defun 360-aicomplete-call-other-backends ()
  "Invoke company completion but disable 360-Aicomplete once, passing query to other backends in `company-backends'.

This is actually obsolete, since `company-other-backend' does the same."
  (interactive)
  (360-aicomplete-with-disabled
   (company-abort)
   (company-auto-begin)))

;;;###autoload
(defun 360-aicomplete (command &optional arg &rest ignored)
  "`company-mode' backend for 360-Aicomplete.

See documentation of `company-backends' for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend '360-aicomplete))
    (prefix (360-aicomplete--prefix))
    (candidates (360-aicomplete--candidates arg))
    (meta (360-aicomplete--meta arg))
    ;(annotation (360-aicomplete--annotation arg))
    ;(post-completion (360-aicomplete--post-completion arg))
    (no-cache t)
    (sorted t)
    (ignore-case 'keep-prefix)))

;;
;; Advices
;;

(defun 360-aicomplete--continue-advice (func &rest args)
  "Advice for `company--continue'."
  (let ((360-aicomplete--calling-continue t))
    (apply func args)))

(advice-add #'company--continue :around #'360-aicomplete--continue-advice)

(defun 360-aicomplete--insert-candidate-advice (func &rest args)
  "Advice for `company--insert-candidate'."
  (if 360-aicomplete-auto-balance
      (let ((smartparens-mode nil))
        (apply func args))
    (apply func args)))

;; `smartparens' will add an advice on `company--insert-candidate' in order to
;; add closing parenthesis.
;; If 360-Aicomplete takes care of parentheses, we disable smartparens temporarily.
(eval-after-load 'smartparens
  '(advice-add #'company--insert-candidate
               :around #'360-aicomplete--insert-candidate-advice))

;;
;; Hooks
;;

(add-hook 'c++-mode-hook (lambda ()
                           (progn (add-to-list (make-local-variable 'company-backends) '360-aicomplete)
                                  (set (make-local-variable 'company-idle-delay)  2)
                                  (set (make-local-variable 'company-minimum-prefix-length)   0)
                                  )))
(message "360 ai init")

(provide '360-aicomplete)

;;; 360-aicomplete.el ends here
