;;; пример от Kpblc
(defun spds-node-select-like (lst             /
                              fun_conv-selset-to-ename        fun_get-node
                              fun_get-sheet   fun_get-scale   fun_get-adress
                              obj             obj_layer       obj_node
                              obj_sheet       obj_adress      obj_scale
                              selset          ent             obj_lst
                              res
                              )
                             ;|
*    Выбор "такой же" узловой выноски СПДС GraphiCS по списку критериев.
*    Параметры вызова:
	lst	список вида
	 '("layer"	; наличие слова указывает, что надо учитывать объекты,
	 		; лежащие на таком же слое
	   "node"	; то же, по номеру узла (в терминологии СПДС)
	   "sheet"	; то же, номеру листа
	   "adress"	; то же, адрес
	   "scale"	; фильтровать по масштабу объекта
	   )
*    Формируется набор только по пространству, в котором лежит "определяющий" объект
*    Примеры вызова:
(spds-node-select-like nil)	; выбрать вообще все.
(spds-node-select-like '("layer")) ; выбрать выноски, лежащие на том же
				; слое, что и "родитель"
(spds-node-select-like '("node"))
(spds-node-select-like '("node" "layer"))
|;

  (defun fun_conv-selset-to-ename (selset / tab item)
    (cond
      ((not selset) nil)
      ((= (type selset) 'pickset)
       (repeat (setq tab  nil
                     item (sslength selset)
                     ) ;_ end setq
         (setq tab (cons (ssname selset (setq item (1- item))) tab))
         ) ;_ end repeat
       )
      ((listp selset) selset)
      ) ;_ end of cond
    ) ;_ end of defun

  (defun fun_get-node (ent_lst)
    (cdr (assoc 300 (member '(301 . "Номер узла") ent_lst)))
	 ; как бы обработать шоб и "ро" и "(з.)" игнорировать в имени выноски
	 ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!????????????????????????
    ) ;_ end of defun

  (defun fun_get-sheet (ent_lst)
    (cdr (assoc 300 (member '(301 . "Номер листа") ent_lst)))
    ) ;_ end of defun

  (defun fun_get-adress (ent_lst)
    (cdr (assoc 300 (member '(301 . "Адрес узла") ent_lst)))
    ) ;_ end of defun

  (defun fun_get-scale (ent_lst)
    (cdr (assoc 40 (member '(301 . "Scale") ent_lst)))
    ) ;_ end of defun
  
(vl-load-com)
  (setq lst (mapcar (function strcase) lst))
  (if (and (= (type (setq obj (vl-catch-all-apply
                                (function
                                  (lambda ()
                                    (ssget "_:E:S")
                                    ) ;_ end of lambda
                                  ) ;_ end of function
                                ) ;_ end of vl-catch-all-apply
                          ) ;_ end of setq
                    ) ;_ end of type
              'pickset
              ) ;_ end of =
           (setq obj (ssname obj 0))
           (= (cdr (assoc 0 (entget obj))) "spdsNoteKnot")
			  (setq selset (ssget "_X" (list (assoc 410 (entget obj))
												(cons -4 "<NOT")
             								(cons 0 "*LINE,*TEXT,*INSERT,CIRCLE,ARC,ELLI*,DIM*,*IM?G*,AEC_*,HATCH")
             								(cons -4 "NOT>")
             								) ;_ end of list
           					) ;_ end of ssget
  				) ;_ end of set
      ) ;_ end of and
    (progn
      (setq obj_lst    (entget obj)
            obj_layer  (if (member "LAYER" lst)
                         (cdr (assoc 8 obj_lst))
                         ) ;_ end of if
            obj_node   (if (member "NODE" lst)
                         (cdr (assoc 300 (member '(301 . "Номер узла") obj_lst)))
                         ) ;_ end of if
            obj_sheet  (if (member "SHEET" lst)
                         (cdr (assoc 300 (member '(301 . "Номер листа") obj_lst)))
                         ) ;_ end of if
            obj_adress (if (member "ADRESS" lst)
                         (cdr (assoc 300 (member '(301 . "Адрес узла") obj_lst)))
                         ) ;_ end of if
            obj_scale  (if (member "SCALE" lst)
                         (cdr (assoc 40 (member '(301 . "Scale") obj_lst)))
                         ) ;_ end of if
            obj_lst    (vl-remove-if-not
                         (function
                           (lambda (x / _lst)
                             (setq _lst (entget x))
                             (and (= (cdr (assoc 0 _lst)) "spdsNoteKnot")
										 	 (if obj_layer
                                    (= (cdr (assoc 8 _lst)) obj_layer)
                                    t
                                    ) ;_ end of if
                                  (if obj_node
                                    (= (fun_get-node _lst) obj_node)
                                    t
                                    ) ;_ end of if
                                  (if obj_sheet
                                    (= (fun_get-sheet _lst) obj_sheet)
                                    t
                                    ) ;_ end of if
                                  (if obj_adress
                                    (= (fun_get-adress _lst) obj_adress)
                                    t
                                    ) ;_ end of if
                                  (if obj_scale
                                    (= (fun_get-scale _lst) obj_scale)
                                    t
                                    ) ;_ end of if
                                  ) ;_ end of and
                             ) ;_ end of lambda
                           ) ;_ end of function
                         (fun_conv-selset-to-ename selset)
                         ) ;_ end of vl-remove-if-not
            res        (ssadd)
            ) ;_ end of setq
      (foreach item obj_lst
        (setq res (ssadd item res))
        ) ;_ end of foreach
      (sssetfirst nil res)
      (sslength res)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun


; запись узл.выноски СПДС, из набора Выноска, в Users1
(defun set-spdsNoteKnotName-to-Users1 ( / selSets selSpdsNode objSpdsNode SpdsNodeName)
  	(vl-load-com)
	(setq selSets (vla-get-SelectionSets (vla-get-ActiveDocument (vlax-get-acad-object))))
	; получаем набор, созданный в VBA
	(setq selSpdsNode (vla-item selSets "Выноска"))
	; получаем объект выноски. (в наборе только один объект)
	(setq objSpdsNode (vla-item selSpdsNode 0))
	;переводим объект в примитив
	(setq objSpdsNode (vlax-vla-object->ename objSpdsNode))
	; получаем значение 300 кода
	(setq SpdsNodeName (cdr (assoc 300 (member '(301 . "Номер узла") (entget objSpdsNode)))))
	; записываем значение в сис.переменную
	(command "_users1" SpdsNodeName)
   (princ)
)

;подсветка набора "Подсветка"
(defun selset-gripset ( / selSets selres selSpdsNode item)
  	(vl-load-com)
	(setq selSets (vla-get-SelectionSets (vla-get-ActiveDocument (vlax-get-acad-object))))
   (setq selSpdsNode (vla-item selSets "Подсветка"))
   (setq selres (ssadd))
   (vlax-for item selSpdsNode
	  (ssadd (vlax-vla-object->ename item) selres)
   )
   (sssetfirst nil selres)
   (princ)
)