;;; ...  -*- lexical-binding: t -*-

(defun rfc8288-parse-header-set-for-links (header-alist &optional uri)
"Parse the Link header fields that a HTTP header set contains.

Given a HEADER-ALIST of (string field-name, string field-value)
pairs, assuming ASCII encoding, it returns a list of link
objects.  If URI is provided, relative urls are resolved relative
to URI."
  (setq uri (or uri ""))
  (let (field-values links)
    ;; 1.  Let field_values be a list containing the members of
    ;;     header_set whose field_name is a case-insensitive match for
    ;;     "link".
    (setq field-values
          (seq-filter (lambda (pair)
                        (equal (downcase (car pair)) "link"))
                      header-alist))

    ;;  2.  Let links be an empty list.
    (setq links '())

    ;; 3. For each field_value in field_values:
    ;;    3.1. Let value_links be the result of Parsing a Link Field
    ;;         Value (Appendix B.2) from field_value.
    ;;    3.2. Append each member of value_links to links.
    (setq links
          (apply #'append
                 links
                 (mapcar
                  (lambda (pair)
                    (setq links
                          (append links
                                  (rfc8288-parse-link-field-value (cdr pair) uri))))
                  field-values)))

    ;;  4.  Return links.
    links))

(defun rfc8288-parse-link-field-value (str &optional uri)
  (setq uri (or uri ""))
  (catch 'return
    ;; 1. Let links be an empty list.
    (let ((chars (append str nil))
          (links nil))

      ;; 2. While field_value has content:
      (while chars
        ;; 2.1. Consume any leading OWS.
        (while (and chars
                    (or (equal (car chars) ?\s)
                        (equal (car chars) ?\t)))
          (setq chars (cdr chars)))

        ;; 2.2. If the first character is not "<", return links.
        (when (or (not chars)
                  (not (equal (car chars) ?\<)))
          (throw 'return links))

        ;; 2.3. Discard the first character ("<").
        (setq chars (cdr chars))

        (let ((target-string '())
              (link-parameters nil)
              (target-uri nil)
              (relations-string nil)
              (relation-types nil)
              (target-attributes nil)
              (context-string nil)
              (context-uri nil))
          ;; 2.4. Consume up to but not including the first ">"
          ;;      character or end of field_value and let the result
          ;;      be target_string.
          (while (and chars
                      (not (equal (car chars) ?\>)))
            (setq target-string (concat target-string
                                        (char-to-string (car chars))))
            (setq chars (cdr chars)))

          (message "found target-string %s" target-string)

          ;; 2.5. If the next character is not ">", return links.
          (unless chars
            (throw 'return links))

          ;; 2.6. Discard the leading ">" character.
          (setq chars (cdr chars))

          ;; 2.7. Let link_parameters be the result of Parsing
          ;;      Parameters (Appendix B.3) from field_value
          ;;      (consuming zero or more characters of it).
          (let ((tmp (rfc8288-parse-parameters chars)))
            (setq chars (car tmp))
            (setq link-parameters (cdr tmp)))

          ;; 2.8. Let target_uri be the result of relatively
          ;;      resolving (as per [RFC3986], Section 5.2)
          ;;      target_string.  Note that any base URI carried in
          ;;      the payload body is NOT used.
          (setq target-uri (rng-uri-resolve target-string uri))

          ;; 2.9. Let relations_string be the second item of the
          ;;      first tuple of link_parameters whose first item
          ;;      matches the string "rel" or the empty string ("")
          ;;      if it is not present.
          (setq relations-string (concat (cdr (assoc "rel" link-parameters #'string=))))

          ;; 2.10. Split relations_string on RWS (removing it in the
          ;;       process) into a list of string relation_types.
          (setq relation-types (split-string relations-string "[\s\t]+" t))

          ;; 2.11. Let context_string be the second item of the
          ;;       first tuple of link_parameters whose first item
          ;;       matches the string "anchor".  If it is not
          ;;       present, context_string is the URL of the
          ;;       representation carrying the Link header
          ;;       [RFC7231], Section 3.1.4.1, serialised as a URI.
          ;;       Where the URL is anonymous, context_string is
          ;;       null.
          (setq context-string (or (cdr (assoc "anchor" link-parameters #'string=))
                                   uri
                                   ""))

          ;; 2.12. Let context_uri be the result of relatively
          ;;       resolving (as per [RFC3986], Section 5.2)
          ;;       context_string, unless context_string is null, in
          ;;       which case context is null.  Note that any base
          ;;       URI carried in the payload body is NOT used.
          (setq context-uri (rng-uri-resolve context-string uri))

          ;; 2.13. Let target_attributes be an empty list.
          ;; NOTE: done in let

          ;; 2.14. For each tuple (param_name, param_value) of link_parameters:

          ;; 2.14.1. If param_name matches "rel" or "anchor", skip
          ;;         this tuple.
          (setq link-parameters
                (seq-remove (lambda (link-parameter)
                              (let ((param-name (car link-parameter)))
                                (or (string-equal param-name "rel")
                                    (string-equal param-name "anchor"))))
                            link-parameters))
          ;; 2.14.2. If param_name matches "media", "title",
          ;;         "title*", or "type" and target_attributes
          ;;         already contains a tuple whose first element
          ;;         matches the value of param_name, skip this
          ;;         tuple.
          (setq link-parameters
                (cl-remove-duplicates
                 link-parameters
                 :test (lambda (a b) (and (string= (car a)
                                              (car b))
                                     (member (car a)
                                             '("media"
                                               "title"
                                               "title*"
                                               "type"))))))
          ;;  2.14.3.  Append (param_name, param_value) to
          ;;           target_attributes.
          (setq target-attributes link-parameters)

          ;; 2.15. Let star_param_names be the set of param_names in the
          ;;       (param_name, param_value) tuples of target_attributes
          ;;       where the last character of param_name is an asterisk
          ;;     ("*").
          ;; NOTE: we don't support any star param
          ;;       internationalisation, so we skip all of these


          ;; 2.16.  For each star_param_name in star_param_names:
          ;; 2.16.1. Let base_param_name be star_param_name with the last
          ;;         character removed.
          ;; 2.16.2. If the implementation does not choose to support an
          ;;         internationalised form of a parameter named
          ;;         base_param_name for any reason (including, but not
          ;;         limited to, it being prohibited by the parameter's
          ;;         specification), remove all tuples from target_attributes
          ;;         whose first member is star_param_name, and skip to the
          ;;         next star_param_name.
          ;; 2.16.3. Remove all tuples from target_attributes whose first
          ;;         member is base_param_name.
          ;; 2.16.4. Change the first member of all tuples in target_attributes
          ;;         whose first member is star_param_name to
          ;;         base_param_name.

          ;; NOTE: we don't support any star param
          ;;       internationalisation, so we skip all of these
          (setq target-attributes
                (remove (lambda (target-attribute)
                          (let ((param-name (car link-parameter)))
                            (string-match-p ".*\\*" param-name)))
                        target-attributes))

          ;; 2.17. For each relation_type in relation_types:
          ;; 2.17.1. Case-normalise relation_type to lowercase.
          (setq relation-types (mapcar #'downcase relation-types))

          ;; 2.17.2. Append a link object to links with the target
          ;;         target_uri, relation type of relation_type,
          ;;         context of context_uri, and target attributes
          ;;         target_attributes.
          (setq links (append links
                              (mapcar (lambda (relation-type)
                                        `((target . ,target-uri)
                                          (relation . ,relation-type)
                                          (context . ,context-uri)
                                          (attrs . ,target-attributes)))
                                      relation-types))))

        ;; TODO: check if/how this ought to be done according to the RFC
        (while (and chars
                    (or (equal (car chars) ?\s)
                        (equal (car chars) ?\t)))
          (setq chars (cdr chars)))

        (when (equal (car chars) ?\,)
          (setq chars (cdr chars))))
      (throw 'return links))))

(defun rfc8288-parse-parameters (str)
  "Takes a string or list of chars, returns a pair of a parsed
parameter list and a list of any remaining, unparsed chars"
  (catch 'return
    ;;  1.  Let parameters be an empty list.
    (let* ((chars (append str nil))
           (parameters nil)
           (consume-ws (lambda ()
                         (while (and chars
                                     (or (equal (car chars) ?\s)
                                         (equal (car chars) ?\t)))
                           (setq chars (cdr chars)))))
           parameter-name
           parameter-value)
      ;; 2. While input has content:
      (while chars

        ;; 2.1. consume any leading ows.
        (funcall consume-ws)

        ;; 2.2. if the first character is not ";", return parameters.
        (when (or (not chars)
                  (not (equal (car chars) ?\;)))
          (throw 'return (cons chars parameters)))

        ;; 2.3. discard the leading ";" character.
        (setq chars (cdr chars))

        ;; 2.4. consume any leading ows.
        (funcall consume-ws)

        ;; 2.5. consume up to but not including the first bws, "=",
        ;;      ";", or "," character, or up to the end of input, and
        ;;      let the result be parameter_name.
        (setq parameter-name "")
        (while (and chars
                    (not (equal (car chars) ?\s))
                    (not (equal (car chars) ?\t))
                    (not (equal (car chars) ?\=))
                    (not (equal (car chars) ?\;))
                    (not (equal (car chars) ?\,)))
          (setq parameter-name (concat parameter-name (char-to-string (car chars))))
          (setq chars (cdr chars)))

        (message "found parameter-name %s" parameter-name)

        ;; 2.6. consume any leading bws.
        (funcall consume-ws)

        ;; 2.7. if the next character is "=":
        (when (and chars (equal (car chars) ?\=))
          ;; 2.7.1.  discard the leading "=" character.
          (setq chars (cdr chars))

          ;; 2.7.2. consume any leading bws.
          (funcall consume-ws)

          (if (and chars (equal (car chars) ?\"))
              ;; 2.7.3.  if the next character is dquote, let
              ;;         parameter_value be the result of parsing a
              ;;         quoted string (appendix b.4) from input
              ;;         (consuming zero or more characters of it).
              (let ((pair (rfc8288-parse-quoted-string chars)))
                (setq chars (car pair))
                (setq parameter-value (cdr pair)))

            ;; 2.7.4.  else, consume the contents up to but not
            ;;         including the first ";" or "," character, or
            ;;         up to the end of input, and let the results
            ;;         be parameter_value.
            (setq parameter-value "")
            (while (and chars
                        (not (equal (car chars) ?\;))
                        (not (equal (car chars) ?\,)))
              (setq parameter-value (concat parameter-value (char-to-string (car chars))))
              (setq chars (cdr chars))))

          ;; 2.7.5. if the last character of parameter_name is an
          ;;        asterisk ("*"), decode parameter_value according
          ;;        to [rfc8187].  continue processing input if an
          ;;        unrecoverable error is encountered.

          ;; note: we don't support star params, so these are ignored
          )
        ;; 2.8. else:
        ;; 2.8.1. let parameter_value be an empty string.
        ;; note: done in let

        ;; 2.9. case-normalise parameter_name to lowercase.
        (setq parameter-name (downcase parameter-name))

        ;; 2.10. append (parameter_name, parameter_value) to parameters.
        (setq parameters (append parameters `((,parameter-name . ,parameter-value))))

        ;; 2.11. consume any leading ows.
        (funcall consume-ws)

        ;; 2.12. if the next character is "," or the end of input, stop
        ;;       processing input and return parameters.
        (when (or (not chars)
                  (equal (car chars) ?\,))
          (throw 'return (cons chars parameters)))))))

(defun rfc8288-parse-quoted-string (str)
  ;; TODO: figure out how this should deal with unescaped commas
  (catch 'return
    (let ((chars (append str nil))
          (output ""))
      ;; 1. Let output be an empty string.
      ;; NOTE: done in let
      ;; 2. If the first character of input is not DQUOTE, return output.
      (unless (equal (car chars) ?\")
        (throw 'return (cons chars output)))

      ;; 3. Discard the first character.
      (setq chars (cdr chars))

      ;; 4. While input has content:
      (while chars
        (cond
         ;; 4.1. If the first character is a backslash ("\"):
         ((equal (car chars) ?\\)
          ;; 4.1.1. Discard the first character.
          (setq chars (cdr chars))

          ;; 4.1.2. If there is no more input, return output.
          (when (not chars)
            (throw 'return (cons chars output)))

          ;; 4.1.3. Else, consume the first character and append
          ;;        it to output.
          (setq output (concat output (char-to-string (car chars))))
          (setq chars (cdr chars)))

         ;; 4.2. Else, if the first character is DQUOTE, discard it
         ;;      and return output.
         ((equal (car chars) ?\")
          (message "Found dquote")
          (setq chars (cdr chars))
          (throw 'return (cons chars output)))

         ;; 4.3. Else, consume the first character and append it to
         ;;      output.
         (t
          (setq output (concat output (char-to-string (car chars))))
          (setq chars (cdr chars)))))

      ;; 5. Return output.
      (throw 'return (cons chars output)))))

(provide 'rfc8288)
