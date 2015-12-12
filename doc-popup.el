;;; doc-popup.el --- Auto documentation popups -*- lexical-binding: t; -*-

;; Copyright (C) Joe Schafer <joe@jschaf.com>

;; Author: Joe Schafer <joe@jschaf.com>
;; URL: https://github.com/jschaf/doc-popup
;; Package-Version: 20151130.58
;; Keywords: tools, convenience
;; Version: 0.1
;; Package-Requires: ((dash "2.12") (pos-tip "0.4.6"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide a documentation popup for the current thing.

;;; Code:

(require 'dash)
(require 'seq)                          ; Sequence functions
(require 'pos-tip)


;;; Customization

(defgroup doc-popup nil
  "Display documentation in tooltips."
  :prefix "doc-popup-"
  :group 'help
  :link '(url-link :tag "Github" "https://github.com/jschaf/doc-popup"))

(defcustom doc-popup-fetchers
  '(emacs-lisp)
  "Documentation fetchers available for automatic selection."
  :group 'doc-popup
  :type '(repeat (symbol :tag "Fetcher"))
  :risky t)

(defcustom doc-popup-show-function #'doc-popup-pos-tip-show
  "A function to show a string in a popup.

The function shall take a single argument, a string, and will
display the string in a graphical popup."
  :group 'doc-popup
  :type 'function
  :package-version '(doc-popup . "0.1"))

(defcustom doc-popup-hide-function #'doc-popup-pos-tip-hide
  "A function to hide the current popup, if any.

The function shall take no arguments and shall hide the current
popup if any.  The function may be called even if there is no
popup being shown; it may not rely upon a popup being present.
The function should be a no-op in this case."
  :group 'doc-popup
  :type 'function
  :package-version '(doc-popup . "0.1"))

(defcustom doc-popup-pos-tip-timeout -1
  "Time in seconds to hide the tooltip after.

If time is non-positive, keep popup displayed until another
command executes, or the popup is closed."
  :group 'doc-popup
  :type 'number
  :package-version '(doc-popup . "0.1"))

(defvar-local doc-popup-fetcher nil
  "Documentation fetcher for the current buffer.")


;;; Documentation fetcher generic definition

(defsubst doc-popup--fetcher-property-name (property)
  "Return the SYMBOL property for fetcher PROPERTY."
  (intern (concat "doc-popup-" (symbol-name property))))

(defun doc-popup-fetcher-get (fetcher property)
  "Get the value of FETCHER's PROPERTY."
  (get fetcher (doc-popup--fetcher-property-name property)))

(gv-define-setter doc-popup-fetcher-get (value fetcher property)
  `(setf (get ,fetcher (doc-popup--fetcher-property-name ,property)) ,value))

(defun doc-popup-define-generic-fetcher (symbol docstring &rest properties)
  "Define SYMBOL as a documentation fetcher.

Any documentation fetcher defined with this macro is eligible for manual
documentation fetcher selection with `doc-popup-select-fetcher'.  To make
the new documentation fetcher available for automatic selection, it must
be registered in `doc-popup-fetchers'.

DOCSTRING is the documentation of the documentation fetcher.  The
following PROPERTIES constitute a documentation fetcher.  Unless
otherwise noted, all properties are mandatory.

`:start FUNCTION'
     A function to start the documentation fetcher.

     FUNCTION shall take two arguments and return a context
     object if the fetcher is started successfully.  Otherwise it
     shall signal an error.

     The first argument is the documentation fetcher being
     started.  The second is a callback function to report state
     changes to Doc-Popup.  The callback takes two arguments
     STATUS DATA, where STATUS is a symbol denoting the documentation
     fetcher status and DATA an optional argument with additional
     data for the status report.  See
     `doc-popup-report-buffer-fetcher-status' for more information
     about STATUS and DATA.

     FUNCTION may be synchronous or asynchronous, i.e. it may
     call the given callback either immediately, or at some later
     point (e.g. from a process sentinel).

     A documentation fetcher _must_ call CALLBACK at least once with a
     STATUS that finishes the current documentation fetcher.  Otherwise
     Doc-Popup gets stuck at the current documentation fetch with this
     documentation fetcher.

     The context object returned by FUNCTION is passed to
     `:interrupt'.

`:interrupt FUNCTION'
     A function to interrupt the documentation check.

     FUNCTION is called with the documentation fetcher and the
     context object returned by the `:start' function and shall
     try to interrupt the documentation check.  The context may
     be nil, if the documentation check is interrupted before
     actually started.  FUNCTION should handle this situation.

     If it cannot interrupt the documentation fetch, it may
     either signal an error or silently ignore the attempt to
     interrupt the documentation fetcher, depending on the
     severity of the situation.

     If interrupting the documentation check failed, Doc-Popup
     will let the documentation check continue, but ignore any
     status reports.  Notably, it won't highlight any errors
     reported by the documentation check in the buffer.

     This property is optional.  If omitted, Doc-Popup won't
     attempt to interrupt documentation checks with this
     documentation fetcher, and simply ignore their results.

`:modes MODES'
    A major mode symbol or list thereof, denoting major modes to
    use this documentation fetcher in.

`:predicate FUNCTION'
    A function to determine whether to use the documentation
    fetcher in the current buffer.

    FUNCTION is called without arguments and shall return non-nil
    if this documentation fetcher shall be used to check the
    current buffer.  Otherwise it shall return nil.

    if `:modes' is also given, FUNCTION is only called in
    matching major modes.

    This property is optional, however at least one of `:modes'
    or `:predicate' must be given."

  (declare (indent 1)
           (doc-string 2))

  (let ((start (plist-get properties :start))
        (interrupt (plist-get properties :interrupt))
        (modes (plist-get properties :modes))
        (predicate (plist-get properties :predicate)))

    (unless (listp modes)
      (setq modes (list modes)))

    ;; TODO: do we want to add :interrupt, :print-doc, :verify, :filter,
    ;; :next-fetchers and :file

    (unless (functionp start)
      (error ":start %S of documentation fetcher %s is not a function" symbol start))
    (unless (or (null interrupt) (functionp interrupt))
      (error ":interrupt %S of documentation fetcher %s is not a function"
             symbol interrupt))
    (dolist (mode modes)
      (unless (symbolp mode)
        (error "Invalid :modes %s in documentation fetcher %s, %s must be a symbol"
               modes symbol mode)))
    (unless (or (null predicate) (functionp predicate))
      (error ":predicate %S of documentation fetcher %s  is not a function"
             symbol predicate))

    (let ((real-predicate
           (lambda ()
             (if (doc-popup-valid-fetcher-p symbol)
                 (or (null predicate) (funcall predicate))
               (lwarn 'doc-popup :warning
                      (concat "%S is not a valid Doc-Popup documentation fetcher.
Try to reinstall the package defining this documentation fetcher.")
                      symbol)
               nil))))

      (pcase-dolist (`(,prop . ,value)
                     `((start         . ,start)
                       (interrupt     . ,interrupt)
                       (modes         . ,modes)
                       (predicate     . ,real-predicate)
                       (documentation . ,docstring)))
        (setf (doc-popup-fetcher-get symbol prop) value)))))


;;; Documentation fetcher selection for the current buffer

(defun doc-popup-may-use-fetcher (fetcher)
  "Whether a generic FETCHER may be used.

Return non-nil if FETCHER may be used for the current buffer, and
nil otherwise."
  (let ((modes (doc-popup-fetcher-get fetcher 'modes))
        (predicate (doc-popup-fetcher-get fetcher 'predicate)))
    (or (not modes) (memq major-mode modes)
        (funcall predicate))))

(defun doc-popup-get-fetcher ()
  "Find the documentation fetcher for the current buffer.

Use the fetcher in the local variable `doc-popup-fetcher' if it
is set.  Otherwise, search for the best fetcher from
`doc-popup-fetchers'."
  (if doc-popup-fetcher
      (if (doc-popup-may-use-fetcher doc-popup-fetcher)
          doc-popup-fetcher
        (error "Doc-popup cannot use %s in this buffer"
               doc-popup-fetcher))
    (seq-find #'doc-popup-may-use-fetcher doc-popup-fetchers)))

(defun doc-popup-valid-fetcher-p (fetcher)
  "Check whether a FETCHER is valid."
  (symbolp fetcher))


;;; Documentation display in popups

(defun doc-popup-pos-tip-show (str)
  "Show a pos-tip popup with STR.

Uses `pos-tip-show' under the hood."
  (pos-tip-show str nil nil nil doc-popup-pos-tip-timeout))

(defun doc-popup-get-doc-at-point ()
  "Get the documentation string for the thing at point."
  )
;;;###autoload
(defun doc-popup-show-at-point ()
  "Show the documentation popup for the thing at point.

Get a documentation fetcher for the current buffer with
  `doc-popup-get-fetcher-for-buffer', and start it."
  (funcall doc-popup-show-function (doc-popup-get-doc-at-point)))

(defun doc-popup-pos-tip-hide ()
  "Hide the `doc-popup' tooltip."
  (pos-tip-hide))



;;; Documentation fetcher definition with elisp functions

;;;###autoload
(defun doc-popup-define-elisp-fetcher (symbol docstring &rest properties)
  "Define SYMBOL as a documentation fetcher using an elisp function.

Define SYMBOL as generic documentation fetcher via
`doc-popup-define-generic-fetcher', which uses an elisp function
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`doc-popup-define-generic-fetcher'.

In addition to the properties understood by
`doc-popup-define-generic-fetcher', the following PROPERTIES
constitute a elisp documentation fetcher.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of elisp fetchers is `doc-popup-sanitize-errors'.

`:elisp FUNCTION'
     The elisp function to run for documentation checking.

     FUNCTION shall take one argument, the string of the thing at
     the point.

Note that you may not give `:start' or `:interrupt' for a elisp
fetcher."

  (declare (indent 1)
           (doc-string 2))
  (dolist (prop '(:start :interrupt))
    (when (plist-get properties prop)
      (error "%s not allowed in definition of elisp documentation fetcher %s"
             prop symbol)))

  (let ((elisp (plist-get properties :elisp))
        (predicate (plist-get properties :predicate)))

    (unless elisp
      (error "Missing :elisp in documentation fetcher %s" symbol))
    (unless (or (symbolp elisp) (listp elisp))
      (error "Elisp for documentation fetcher %s must be a string: %S"
             symbol elisp))

    (setq properties
          (plist-put properties :predicate
                     (lambda ()
                       (or (not predicate) (funcall predicate)))))

    (apply #'doc-popup-define-generic-fetcher symbol docstring
           :start #'doc-popup-start-elisp-fetcher
           :interrupt #'doc-popup-interrupt-elisp-fetcher
           properties)

    (pcase-dolist (`(,prop . ,value)
                   `((elisp . ,elisp)))
      (setf (doc-popup-fetcher-get symbol prop) value))))

(defun doc-popup-start-elisp-fetcher (fetcher callback)
  "Start an elisp FETCHER with CALLBACK."

  )

(defun doc-popup-interrupt-elisp-fetcher (_fetcher process)
  "Start an elisp FETCHER with CALLBACK."

  )






;;; Documentation display in popups

(defun doc-popup-pos-tip-show (str)
  "Show a pos-tip popup with STR.

Uses `pos-tip-show' under the hood."
  (pos-tip-show str nil nil nil doc-popup-pos-tip-timeout))

(defun doc-popup-get-doc-at-point ()
  "Get the documentation string for the thing at point."
  )
;;;###autoload
(defun doc-popup-show-at-point ()
  "Show the documentation popup for the thing at point."
  (funcall doc-popup-show-function (doc-popup-get-doc-at-point)))

(defun doc-popup-pos-tip-hide ()
  "Hide the `doc-popup' tooltip."
  (pos-tip-hide))







(provide 'doc-popup)

;;; doc-popup.el ends here
