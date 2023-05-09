;;; package --- Summary

;;; Commentary:

;;; Code:

(add-to-list 'load-path "./")
(require 'undercover nil t)
(undercover "*.el" (:report-format 'simplecov) (:send-report nil))

(provide 'test-helper)
;;; test-helper.el ends here

