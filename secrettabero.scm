;;
;; Copyright 2023, Jaidyn Ann <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;

(import (prefix dbus dbus:) sxml-serializer)


;; Debugging
(dbus:default-signal-handler (lambda (ctx mber args)
        ((dbus:printing-signal-handler) ctx mber args)
        (dbus:dump-callback-table)))



;; —————————————————————————————————————
;; Utilities
;; —————————————————————————————————————

;; Generate `Introspect`-form XML for a DBUS node, given its interfaces.
;; The interfaces should be in the format:
;; (INTERFACE-NAME . ((METHOD-NAME-A . ARGUMENT-ALISTS-A)
;;                    (METHOD-NAME-B . ARGUMENT-ALISTS-B)
;;                    ...))
;; The argument alists should have the traits 'name, 'direction, and 'type.
(define (introspect-node-xml interfaces #!optional (subnodes '()))
  (string-append
   "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
\"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">\n\n"
   (serialize-sxml (introspect-node-sxml interfaces subnodes)
				  #f)))


;; Generate a node's list containing the given interfaces, for use with SXML for
;; serialization to `Introspect`-friendly XML.
;; See `introspect-node-xml` for a description of the structure.
(define (introspect-node-sxml interfaces #!optional (subnodes '()))
  (append (list 'node)
		  (map (lambda (interface-list)
				 (introspect-interface-sxml (car interface-list)
											(cdr interface-list)))
			   interfaces)
		  (map (lambda (subnode)
				 `(node (@ (name ,subnode))))
			   subnodes)))


;; Generate an interface's list of the given name and methods,
;; for use with SXML for serialization to `Introspect`-friendly XML.
;; The method-lists are of the format:
;;    (METHOD-NAME . ((ARGUMENT-ALIST-A)
;;                    (ARGUMENT-ALIST-B)
;;                    ...))
;; Where the argument alists are alists containing the traits 'name,
;; 'direction, and 'type.
(define (introspect-interface-sxml interface-name method-lists)
  (append
   `(interface (@ (name ,interface-name)))
   (map (lambda (method-list)
		  (introspect-method-sxml (car method-list)
								  (cdr method-list)))
		method-lists)))


;; Generate an interface's list of the given name and methods,
;; for use with SXML for serialization to `Introspect`-friendly XML.
;; The method-lists are of the format:
;;    (METHOD-NAME . ((ARGUMENT-ALIST-A)
;;                    (ARGUMENT-ALIST-B)
;;                    ...))
;; An argument alist contains the traits 'name, 'direction, and 'type.
(define (introspect-method-sxml name arg-alists)
  (append
   `(method
	 (@ (name ,name)))
   (map (lambda (arg-alist)
		  (introspect-method-arg-sxml (alist-ref 'name arg-alist)
									  (alist-ref 'type arg-alist)
									  (alist-ref 'direction arg-alist)))
		arg-alists)))


;; Generate an argument's list for use with SXML for serialization
;; into XML, to create an <argument> tag.
;; The `name` can be arbitrary, `type` can be (i.e.) "s", and
;; `direction` is either "in" or "out".
(define (introspect-method-arg-sxml name type direction)
  `(arg (@ (name ,name)
		   (type ,type)
		   (direction ,direction))))



;; —————————————————————————————————————
;; DBUS generally
;; —————————————————————————————————————

;; Creates an Introspectable interface for the given path, given a list
;; of interfaces and subnodes.
(define-syntax handle-introspection-for-path
  (syntax-rules ()
	((handle-introspection-for-path node-path interfaces subnodes)
	 (let* ([xml
			 (introspect-node-xml
			  (list
			   (append '("org.freedesktop.DBus.Introspectable"
						 . (("Introspect"
							 . (((direction . "out")
								 (type . "s")
								 (name . "xml_data"))))))
					   interfaces))
			  subnodes)]
			[context
			 (dbus:make-context bus: dbus:session-bus
								service: 'org.jadedctrl.secrettabero
								interface: 'org.freedesktop.DBus.Introspectable
								path: node-path)])
	   (dbus:register-method context "Introspect"
							 (lambda () xml))))))



;; —————————————————————————————————————
;; DBUS Path: /
;; —————————————————————————————————————

(handle-introspection-for-path '/ '() '(org))



;; —————————————————————————————————————
;; DBUS Path: /org/
;; —————————————————————————————————————

(handle-introspection-for-path '/org '() '(freedesktop))



;; —————————————————————————————————————
;; DBUS Path: /org/freedesktop/
;; —————————————————————————————————————

(handle-introspection-for-path '/org/freedesktop '() '(secrets))



;; —————————————————————————————————————
;; DBUS Path: /org/freedesktop/secrets
;; —————————————————————————————————————

(handle-introspection-for-path '/org/freedesktop/secrets '() '())



;; —————————————————————————————————————
;; Invocation
;; —————————————————————————————————————

(let loop ()
	; (printf "poll~%")
	; (dbus:poll-for-message bus: dbus:session-bus)	;; would be the session-bus by default anyway
	(dbus:poll-for-message)
	(loop))


