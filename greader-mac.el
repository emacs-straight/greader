(defgroup greader-mac
  nil
  "Back-end of mac for greader."
  :group 'greader
  )

(defcustom greader-mac-voice nil
  "Set the desired voice for the tts `say’.
nil means to use the system voice."
  :tag "mac tts voice"
  :type '(choice (const :tag "system voice" nil)
		 (string :tag "Specify")))

(defcustom greader-mac-rate 200
  "Rate of tts expressed in words per minute."
  :tag "Mac tts speech rate"
  :type 'integer)

(defcustom greader-mac-executable-name "say"
  "Name of the program that actually produces the voice."  
  :tag "mac tts command"
  :type 'string)

(defvar greader-mac-end-of-sentence-regexp "[.?!,][«»\"]?[\n ]"
  "A regexp that matches the end of the string to send to the tts.")

(defun greader-mac-set-rate (&optional rate)
  "Return a string suitable for setting mac RATE."
  (if (not rate)
      (concat "-r" (number-to-string greader-mac-rate))
    (progn
      (setq-local greader-mac-rate rate)
      (concat "-r" (number-to-string rate)))))

(defun greader-mac-set-voice (voice)
  "Set specified VOICE for `say'.
When called interactively, this function reads a string from the minibuffer providing completion."
  (interactive
   (list (read-string "voice: " nil nil (greader--mac-get-voices))))
  (let (result)
    (if (called-interactively-p 'any)
	(progn
	  (if (string-equal "system" voice)
	      (setq-local greader-mac-voice nil)
	    (setq-local greader-mac-voice voice)))
      (when voice
	(if (string-equal voice "system")
	    (progn
	      (setq result nil)
	      (setq-local greader-mac-voice nil))
	  (setq result (concat "-v" voice))
	  (setq-local greader-mac-voice voice)))
      (unless voice
	(if greader-mac-voice
	    (setq result (concat "-v" greader-mac-voice))
	  (setq result nil)))
      result)))

;;;###autoload
(defun greader-mac (command &optional arg &rest _)
  "Back-end main function of greader-mac.
COMMAND must be a string suitable for `make-process'."
  (pcase command
    ('get-text
     (greader-mac-get-sentence))
    ('next-text
     (greader-mac-forward-sentence))
    ('executable
     greader-mac-executable-name)
    ('lang
     (greader-mac-set-voice arg))
    ('set-voice
     (call-interactively 'greader-mac-set-voice))
    ('rate
     (cond
      ((equal arg 'value)
       greader-mac-rate)
      (t
       (greader-mac-set-rate arg))))
    ('punctuation
     nil)
    (_
     'not-implemented)))
(put 'greader-mac 'greader-backend-name "greader-mac")

(defun greader-mac-get-sentence ()
  (let ((sentence-start (make-marker)))
    (setq sentence-start (point))
    (save-excursion
      (when (not (eobp))
	(if (not (re-search-forward greader-mac-end-of-sentence-regexp nil t))
	    (end-of-buffer))
      (if (> (point) sentence-start)
	  (string-trim (buffer-substring-no-properties sentence-start (point)) "[ \t\n\r]+")
	nil)))))

(defun greader-mac-forward-sentence ()
  (if (not (re-search-forward greader-mac-end-of-sentence-regexp nil t))
      (end-of-buffer)))

(defun greader--mac-get-voices ()
  "Return a list which contains all voices suitable for this backend."
  (with-temp-buffer
    (call-process "say" nil t nil "-v" "?")
    (beginning-of-buffer)
    (let ((lines (list "system")))
      (while (not (eobp))
	(let ((mymarker (make-marker)))
	  (search-forward "_")
	  (backward-word)
	  (re-search-backward "[^ ]")
	  (forward-char)
	  (set-marker mymarker (point))
	  (end-of-line)
	  (delete-region mymarker (point)))	  
	(push (string-chop-newline (thing-at-point 'line)) lines)
	(forward-line))
      (reverse lines))))

(provide 'greader-mac)
