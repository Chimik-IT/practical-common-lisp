;;; NMAP XML Reader for Emacs

(defun set-file-paths ()
  (prin1 "Enter path to nmap xml:")
  (setq xmlfile (read))
  (prin1 "Enter path to Hosts CSV-file:")
  (setq	csvfile (read))
  (prin1 "Paths stored!"))
(defvar csvfile "~/org/scripts/my-table-parser/table.csv")
(defvar xmlfile "~/telekom2.xml")
(defun map-xml-to-lisp ()
  (let* ((nmap-xml-object (with-temp-buffer
			    (insert-file-contents xmlfile)
			    (libxml-parse-xml-region (point-min) (point-max))))
	 (host-nodes (dom-by-tag nmap-xml-object 'host)))
    host-nodes))

(defun get-csv-content ()
  (with-temp-buffer
    (insert-file-contents "~/org/scripts/my-table-parser/table.csv")
    (split-string (buffer-string) "\n")))

(defun content2-list-of-lines ()
  (setq list-of-lines ()
	list-of-strings (get-csv-content))
  (while list-of-strings
    (push (split-string (pop list-of-strings) ",") list-of-lines))
  list-of-lines)

(defun search-hostname (ip)
  (setq search-list (content2-list-of-lines))
  (setq line-number1 (address2number ip))
  (cadr (elt search-list line-number1)))

(defun make-ip-list ()
  (setq ip-list ())
  (dolist (i (content2-list-of-lines))
    (push (car i) ip-list))
  (setq ip-list (reverse (cdr (reverse ip-list)))))

(defun address2number (ip)
  (string-to-number (nth 3 (split-string ip "\\."))))

(defun combine2lists (list1 list2 combinator)
  (setq serv ())
  (while list1
    (setq item1 (pop list1)
	  item2 (pop list2))
    (if (string= item1 "nil")
	(setq item1 ""
	      item2 "")
      (push (format "%s%s%s" item1 combinator item2) serv)))
  (mapconcat 'identity serv " "))

(defun get-status (ip)
  (cdaadr (assoc 'status (elt (map-xml-to-lisp) (address2number ip)))))

(defun get-hostname (ip)
  (if (string= (cdaadr (assoc 'hostname (assoc 'hostnames (elt (map-xml-to-lisp) (address2number ip))))) "nil")
      (search-hostname ip)
    (cdaadr (assoc 'hostname (assoc 'hostnames (elt (map-xml-to-lisp) (address2number ip)))))))

(defun make-service-list (ip)
  (setq length-ports-list (length (assoc 'ports (elt (map-xml-to-lisp) (address2number ip))))
	port-list (assoc 'ports (elt (map-xml-to-lisp) (address2number ip)))
	num 2
	services-list ()
	serv-name-list ())
  (while (< num length-ports-list)
    (setq num (1+ num))
    (push (cdr (assoc 'portid (cadr(elt (assoc 'ports (elt (map-xml-to-lisp) (address2number ip)))num)))) services-list)
    (push (cdaadr (assoc 'service (cdddr (elt (assoc 'ports (elt (map-xml-to-lisp) (address2number ip))) num)))) serv-name-list))
  (setq service-port-list (combine2lists serv-name-list services-list "/")))

(defun make-service-csv (ip-list)
  (setq hosts-list ())
  (while ip-list
    (setq ip (pop ip-list)
	  hostname (get-hostname ip)
	  status (get-status ip)
	  services (make-service-list ip))
    (push (list ip hostname status services) hosts-list))
  (reverse hosts-list))

(defun write-to-csv-file (lst)
  (while lst
    (write-region (concatenate 'string (mapconcat 'identity (pop lst) ",") "\n") nil "service.csv" t)))
