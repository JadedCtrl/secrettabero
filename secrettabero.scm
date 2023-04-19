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

(import (chicken io)
		(prefix srfi-13 13:)
		(prefix dbus dbus:)
		(prefix sxml-serializer sxml:))


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
   (sxml:serialize-sxml (introspect-node-sxml interfaces subnodes)
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

;; Definition of org.freedesktop.DBus.Introspectable, in
;; `introspect-interface-sxml`-compliant format.
(define *introspectable-interface*
  '("org.freedesktop.DBus.Introspectable"
	. (("Introspect"
		. (((direction . "out")
			(type . "s")
			(name . "xml_data")))))))


;; Definition of org.freedesktop.DBus.Peer, in
;; `introspect-interface-sxml`-compliant format.
(define *peer-interface*
  '("org.freedesktop.DBus.Peer"
	. (("GetMachineId"
		. (((direction . "out")
			(type . "s")
			(name . "machine_uuid")))))))


;; Creates an org.freedesktop.DBus.Introspectable interface for the given
;; path, given a list of interfaces and subnodes.
;; Subnodes ought be a list of names e.g., '(subnode-a subnode-b), but
;; interfaces ought be in a specific format; see the comment on
;; `introspect-interface-sxml` for details.
(define-syntax handle-introspection-for-path
  (syntax-rules ()
	((handle-introspection-for-path node-path interfaces subnodes)
	 (let* ([xml
			 (introspect-node-xml
			  (append
			   (list *introspectable-interface*)
			   interfaces)
			  subnodes)]
			[context
			 (dbus:make-context bus: dbus:session-bus
								service: 'org.jadedctrl.secrettabero
								interface: 'org.freedesktop.DBus.Introspectable
								path: node-path)])
	   (dbus:register-method context "Introspect"
							 (lambda () xml))))))


;; Creates a org.freedesktop.DBus.Peer interface along the given path.
(define-syntax handle-peers-for-path
  (syntax-rules ()
	[(handle-peers-for-path node-path)
	 (let ([machine-id
			(13:string-delete #\newline
							  (call-with-input-file "/var/lib/dbus/machine-id"
								(lambda (in-port)
								  (read-string #f in-port))))]
		   [context
			(dbus:make-context bus: dbus:session-bus
							   service: 'org.jadedctrl.secrettabero
							   interface: 'org.freedesktop.DBus.Peer
							   path: node-path)])
	   (dbus:register-method context "Ping"
							 (lambda () '()))
	   (dbus:register-method context "GetMachineId"
							 (lambda () machine-id)))]))




;; —————————————————————————————————————
;; DBUS Path: /
;; —————————————————————————————————————

(handle-peers-for-path '/)
(handle-introspection-for-path '/ (list *peer-interface*) '(org))



;; —————————————————————————————————————
;; DBUS Path: /org/
;; —————————————————————————————————————

(handle-peers-for-path '/org)
(handle-introspection-for-path '/org (list *peer-interface*) '(freedesktop))



;; —————————————————————————————————————
;; DBUS Path: /org/freedesktop/
;; —————————————————————————————————————

(handle-peers-for-path '/org/freedesktop)
(handle-introspection-for-path '/org/freedesktop (list *peer-interface*) '(secrets))



;; —————————————————————————————————————
;; DBUS Path: /org/freedesktop/secrets
;; —————————————————————————————————————

(handle-peers-for-path '/org/freedesktop/secrets)
(handle-introspection-for-path '/org/freedesktop/secrets (list *peer-interface*) '())



;; —————————————————————————————————————
;; Invocation
;; —————————————————————————————————————

(let loop ()
	; (printf "poll~%")
	; (dbus:poll-for-message bus: dbus:session-bus)	;; would be the session-bus by default anyway
	(dbus:poll-for-message)
	(loop))

