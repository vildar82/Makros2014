(princ "\n ---- MIP Pltools loading")

;|Программы и панели взяты отсюда
http://dwg.ru/forum/viewtopic.php?t=8509
http://www.autocad.ru/cgi-bin/f1/board.cgi?t=23073xg
http://www.autocad.ru/cgi-bin/f1/board.cgi?t=20156yO&page=4
|;
(vl-load-com)
(or *pl-activedoc* 
    (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
(setq *pl:IsRus* (= (getvar "DWGCODEPAGE") "ANSI_1251"))
;;; ============ Библиотечные ф-ции BEGIN =============================

;|=============================================================================
*    Функция преобразования полученного значения в ename
*    Параметры вызова:
*	ent_value	значение, которое надо преобразовать в примитив. Может
*			быть:
*                       -    именем примитива,
*                       -    vla-указателем,
*                       -    меткой,
*                       -    спиком entget,
*                       -    спиком entsel.
*			Если не принадлежит ни одному из указанных типов,
*			возвращается nil
*    Примеры вызова:
(pl:conv-ent-to-ename (entlast))
(pl:conv-ent-to-ename (entget(entlast)))
(pl:conv-ent-to-ename (cdr(assoc 5 (entget(entlast)))))
(pl:conv-ent-to-ename (car(entsel)))
(pl:conv-ent-to-ename (vlax-ename->vla-object (entlast)))
=============================================================================|;
(defun pl:conv-ent-to-ename (ent_value / ret)
  (cond
    ((= (type ent_value) 'vla-object) (vlax-vla-object->ename ent_value))
    ((= (type ent_value) 'ename) ent_value)
    ((and (= (type ent_value) 'list)
          (= (type (setq ret (car ent_value))) 'ename)
          )
     ret
     )
    ((and (= (type ent_value) 'str)(setq ret (handent ent_value))) ret)
    ((= (type ent_value) 'list)(cdr (assoc -1 ent_value)))
    (t nil)
    ) ;_ end of cond
  ) ;_ end of defun

;|=============================================================================
*    Функция преобразования полученного значения в vla-указатель.
*    Параметры вызова:
*	ent_value	значение, которое надо преобразовать в примитив. Может
*			быть:
*                       -    именем примитива,
*                       -    vla-указателем,
*                       -    меткой,
*                       -    спиком entget,
*                       -    спиком entsel.
*			Если не принадлежит ни одному из указанных типов,
*			возвращается nil
*    Примеры вызова:
(pl:conv-ent-to-vla (entlast))
(pl:conv-ent-to-vla (entget(entlast)))
(pl:conv-ent-to-vla (cdr(assoc 5 (entget(entlast)))))
(pl:conv-ent-to-vla (car(entsel)))
(pl:conv-ent-to-vla (vlax-ename->vla-object (entlast)))
=============================================================================|;
(defun pl:conv-ent-to-vla (ent_value / ret)
  (cond
    ((= (type ent_value) 'vla-object) ent_value)
    ((= (type ent_value) 'ename) (vlax-ename->vla-object ent_value))
    ((setq ret (pl:conv-ent-to-ename ent_value))(vlax-ename->vla-object ret))
    (t nil)
    ) ;_ end of cond
  ) ;_ end of defun


;;;Ф-ция изменяет i-й(начиная с 0) элемент списка новым значением
;;; i - индекс элемента itm - значение  lst - список
(defun pl:subst-i (i itm lst)
  (setq i (1+ i))
  (mapcar
    '(lambda (x)
      (if (zerop (setq i (1- i))) itm x)
    )
    lst
  )
)

;|============================================================================= 
*    Функция преобразования набора, полученного через (ssget), в список 
* ename-примитивов. 
*    Параметры вызова: 
*   value   набор примитивов 
*    Примеры вызова: 
(pl:selset-to-enamelist (ssget)) 
=============================================================================|; 
(defun pl:selset-to-enamelist (value / item lst)
       (repeat (setq item (sslength value)) ;_ end setq
         (setq lst (cons (ssname value (setq item (1- item))) lst))
         ) ;_ end repeat

  
;;;  (if selset 
;;;    (vl-remove-if 'listp (mapcar 'cadr (ssnamex selset))) 
;;;    )
  ) ;_ end of defun 

;|============================================================================= 
*    Функция преобразования набора, полученного через (ssget), в список 
* vla-примитивов. 
*    Параметры вызова: 
*   selset   набор примитивов 
*    Примеры вызова: 
(pl:selset-to-vlalist (ssget)) 
=============================================================================|; 
(defun pl:selset-to-vlalist(selset) 
  (if selset 
    (mapcar 'vlax-ename->vla-object
            (pl:selset-to-enamelist selset) 
            ))) ;_ end of defun




;;;http://www.arcada.com.ua/forum/viewforum.php?f=24&sid=b76b98080333c300c1c4e108effc725f
;;;*    Восстанавливаются системные переменные. Значения системных переменных
;;;* должны храниться в глобальном списке *pl-sysvar-list*. Если списка нет
;;;* (nil), происходит просто выход.
;;;*    Параметры вызова:
;;;*  Нет
;;;*    Примеры вызова:
;;;(pl-error-restore-sysvar)

(defun pl-error-restore-sysvar ()
  (if *pl-sysvar-list*
    (foreach item *pl-sysvar-list* (_pl-sysvar-set (car item) (cadr item))))
  (setq *pl-sysvar-list* nil)(gc)) ;_ end of defun

;;;http://www.arcada.com.ua/forum/viewforum.php?f=24&sid=b76b98080333c300c1c4e108effc725f
;;;*    Сохраняется текущее значение системных переменных. Список
;;;глобальный (*pl-sysvar-list*)
;;;* При условии, что заданы значения, они устанавливаются.
;;;*    Поскольку список *pl-sysvar-list* не обнуляется, в нем
;;;хранится история изменения
;;;* значений переменных.
;;;*    Параметры вызова:
;;;*  *pl-sysvar-list*  список системных переменных, состояние
;;;которых надо сохранить.
;;;*      Список состоит из подсписков (Переменная Значение)
;;;*      В списке могут повторяться Переменные. В таком случае будет
;;;*      установлено последнее значение.
;;;*      Если в качестве второго параметра используется nil, то значение
;;;*      системной переменной просто сохраняется.
;;;*    Примеры вызова:
;;;(kpblc-error-sysvar-list (list '("cmdecho" 0) '("blipmode") '("osmode" 503)))

(defun pl-error-save-sysvar (sysvar-list)
  (foreach item  sysvar-list
    (setq *pl-sysvar-list*
     (cons (list (car item) (getvar (car item))) *pl-sysvar-list*))
    (if  (cadr item)(_pl-sysvar-set (car item) (cadr item))) ;_ end of if
    ) ;_ end of foreach
  ) ;_ end of defun


;;;*    Установка системных переменных. Замена стандартному (setvar) для 
;;;* безошибочной обработки 
;;;*    Параметры вызова: 
;;;*   sysvar   имя системной переменной 
;;;*   value   устанавливаемое значение 
;;;*    Возвращаемое значение: 
;;;*   Установленное значение системной переменной либо nil в случае неудачи 

(defun _pl-sysvar-set (sysvar value) 
  (if (getvar sysvar)         ; Такая переменная есть вообще или нет? 
    (if   (and (= value "")(wcmatch (strcase sysvar t) "dim*")) ;_ end of and 
      (setvar sysvar ".")(vl-catch-all-apply 'setvar (list sysvar value))))
  (getvar sysvar)) ;_ end of defun

;============== pl:GetInters ====================
;*  Возвращает все пересечения между двумя объектами
;*  Один из элементов может быть блоком
;   Arguments [Type]:
;   FstO = Первый объет  [VLA-OBJECT]
;   NxtO = Второй объект [VLA-OBJECT]
;   Mde = Тип пересечения [INT]
;         Constants:
;         - acExtendNone           Does not extend either object.
;         - acExtendThisEntity     Extends the FstO object.
;         - acExtendOtherEntity    Extends the NxtO object.
;         - acExtendBoth           Extends both objects.
;  Возвращает [Type]:
;   > Список точек '((1.0 1.0 0.0)... [LIST]
;   > Nil если пересечений не найдено
; Notes:
;   - Один из элементов должен быть не блок
;======================================================
(defun pl:GetInters (FstO NxtO Mde / IntLst PtLst)
 (cond
   ((and (= (vla-get-ObjectName FstO) "AcDbBlockReference")
	 (not (vlax-property-available-p FstO 'Path))) ;_not Xref
    (setq PtLst (lib:GetBlockInters FstO NxtO Mde)))
   ((and (= (vla-get-ObjectName NxtO) "AcDbBlockReference")
	 (not (vlax-property-available-p NxtO 'Path))) ;_not Xref
    (setq PtLst (lib:GetBlockInters NxtO FstO Mde)))
   ((or ;_Xref
    (vlax-property-available-p FstO 'Path)
    (vlax-property-available-p NxtO 'Path))
    (setq PtLst nil))
   (t
    (setq IntLst (vlax-invoke FstO 'IntersectWith NxtO Mde))
    (if IntLst
      (setq PtLst (pl:group-by-num IntLst 3))
      (setq PtLst nil))
    ))
  PtLst
  )

(defun pl:mydcl (zagl info-list / fl ret dcl_id)
    (vl-load-com)
    (if (null zagl)
        (if *pl:IsRus* (setq zagl "Выбор")(setq zagl "Select"))
    ) ;_ end of if
    (setq fl (vl-filename-mktemp "mip" nil ".dcl"))
    (setq ret (open fl "w"))
    (mapcar '(lambda (x) (write-line x ret))
            (list "mip_msg : dialog { "
                  (strcat "label=\"" zagl "\";")
                  " :list_box {"
                  "alignment=top ;"
                  "width=51 ;"
                  (if (> (length info-list) 26)
                      "height= 26 ;"
                      (strcat "height= " (itoa (+ 3 (length info-list))) ";")
                  ) ;_ end of if
                  "is_tab_stop = false ;"
                  "key = \"info\";}"
                  "ok_cancel;}"
            ) ;_ end of list
    ) ;_ end of mapcar
    (setq ret (close ret))
    (if (setq dcl_id (load_dialog fl))
        (if (new_dialog "mip_msg" dcl_id)
            (progn
                (start_list "info")
                (mapcar 'add_list info-list)
                (end_list)
                (set_tile "info" "0")
                (setq ret (car info-list))
                (action_tile "info" "(setq ret (nth (atoi $value) info-list))")
                (action_tile "cancel" "(progn(setq ret nil)(done_dialog 0))")
                (action_tile "accept" "(done_dialog 1)")
                (start_dialog)
            ) ;_ end of progn
        ) ;_ end of if
    ) ;_ end of if
    (unload_dialog dcl_id)
    (vl-file-delete fl)
    ret
) ;_ end of defun

(defun pl:str-str-lst (str pat / i)
  (cond ((= str "") nil)
        ((setq i (vl-string-search pat str))
         (cons (substr str 1 i)
               (pl:str-str-lst (substr str (+ (strlen pat) 1 i)) pat)
         ) ;_  cons
        )
        (t (list str))
  ) ;_  cond
) ;_  defun

;=============================================================================
;*    Создание сегментов
;*    Параметры вызова:
;*    lst-point — список точек
;*    Примеры вызова:
;*   (pl:mkseg '(1 2 3 4))
;*  Результат: ((1 2) (2 3) (3 4))
;==============================================================================;
(defun pl:mkseg ( lst / ret z1 z2)
    (setq z1 (car lst))
    (foreach z2 (cdr lst)
        (setq ret (cons (list z1 z2) ret)
              z1 z2)
        )
  (reverse ret)    
    
) ;_ end of defun

  

;;;*                pl:Is-object-modifed
;;;*  проверка обьекта на возможность модификации параметра
;;;*  и lock слоя
;;;*  Параметры вызова:
;;;*   object - Vla-object
;;;*   param — изменяемое свойство [string] или nil (проверяется layer)
;;;*   Воззврат:
;;;*   T  - объект можно изменять
;;;*   nil - нет
;;;*   Пример вызова
;;;*   (pl:Is-object-modifed (vlax-ename->vla-object (entlast)) nil)
(defun pl:Is-object-modifed (object param)
    (if (= (type object) 'ENAME)(setq object (vlax-ename->vla-object object)))
    (and (or param (setq param '"layer"))
	 (vlax-write-enabled-p object)
	 (vlax-property-available-p object param t)
         )
    )
    
;;;Удаляет одинаковые (дубликаты) элементы из списка
(defun pl:MakeUniqueMembersOfList  ( lst / OutList head)
  (while lst
    (setq head (car lst)
          lst (vl-remove-if '(lambda(pt)(equal pt head 1e-6)) lst)
          OutList (append OutList (list head))))
  OutList
  )
  
;;;Ф-ция переводит градусы в радианы
;;;( pl:DTR a)
(defun pl:DTR (a)(* pi (/ a 180.0)))
;;;---------------------------------------------
;;;Ф-ция переводит радианы в градусы
;;;( R2D a)
(defun pl:RTD (a)(/ (* a 180.0) pi))

;;; ============ Библиотечные ф-ции END =============================

;;; ============ Библиотечные ф-ции POLYLINE BEGIN =============================


;;;* Ф-ция lib:pline-get-verts
;;;* Возвращает координаты вершин полилинии
;;;* Взята или на autocad.ru или на dwg.ru 
;;;* Arguments [Type]:
;;;   pline_obj = Object [Vla-Object]
;;;* Возвращает [Type]:
;;;   список координат вида ((90.987 183.524) (93.2774 206.991) (123.052 208.708) (140.23 184.382) (111.6 170.073))


(defun lib:pline-get-verts (pline_obj)
  (if (= (type pline_obj) 'Ename)
    (setq pline_obj (vlax-ename->vla-object pline_obj))
  ) ;_ end of if
      (cond
        ((wcmatch (vlax-get pline_obj 'Objectname )
           "AcDb2dPolyline,AcDb3dPolyline")
         (pl:group-by-num (vlax-get pline_obj 'Coordinates) 3)
        )
        ((eq (vlax-get pline_obj 'Objectname )
           "AcDbPolyline")
         (pl:group-by-num (vlax-get pline_obj 'Coordinates) 2)
        )
       ((eq (vlax-get pline_obj 'Objectname )
           "AcDbLine")
         (list (vlax-curve-getstartpoint pline_obj)(vlax-curve-getendpoint pline_obj))
        )

        (T nil)))

;********************************
; Векторное произведение векторов
;********************************
; W1, W2 - вектора
; Возвращает: вектор нормали к плоскости заданной векторами  в правой системе координат.
;W1 и W2 не должны лежать на одной прямой).
(defun pl:3d_Wnorm (W1 W2)
  (if (< (length W1) 3)(setq W1 (list (car W1)(cadr W1) 0)))
  (if (< (length W2) 3)(setq W2 (list (car W2)(cadr W2) 0)))
  (list (- (* (cadr W1)(caddr W2))(* (caddr W1)(cadr W2)))
        (- (* (caddr W1)(car W2)) (* (car W1)(caddr W2)))(- (* (car W1)(cadr W2)) (* (cadr W1)(car W2)))))




;;;* Ф-ция _pline-get-radii
;;;* Возвращает радиус дугового сегмента полилинии
;;;* get bulge radius
;;;* math by Juergen Menzi
;;;* Arguments [Type]:
;;;   p1 = Point [LIST] - точка начала сегмента
;;;   p2 = Point [LIST] - точка конца сегмента
;;; bulge = кривизна
;;;* Возвращает [Type]:
;;;   радиус [Real]
(defun lib:pline-get-radii  (p1 p2 bulge)
 (if (not(zerop bulge))   
  (abs (/ (/ (distance p1 p2) 2.0)
    (sin (* 2.0 (atan bulge)))))
     0.0))

(defun lib:pline-get-segm-center  (pline p1 p2 bulge / cpt midc midp rad)
(setq rad (lib:pline-get-radii p1 p2 bulge)
      midp (vlax-curve-getpointatparam pline
       (+ (fix (vlax-curve-getparamatpoint pline p1)) 0.5))
      midc (mapcar (function (lambda (x y)(/ (+ x y) 2))) p1 p2)
      cpt (trans (polar midp (angle midp midc) rad) 0 1)
)
cpt
)

;;======================================================;;
;;  written by Fatty The Old Horse 10/13/05    ;;
;;      (framework)      ;;
;;======================================================;;
;;      helper functions  ;;
;; group list by number
(defun pl:group-by-num (lst num / ls ret)
(if (= (rem (length lst) num ) 0)
 (progn (setq ls nil)
  (repeat (/ (length lst) num)
    (repeat num (setq ls (cons (car lst) ls) lst (cdr lst)))
  (setq ret (append ret (list (reverse ls))) ls nil)))) ret)

;;;Реверс LW полилиний
;;;Код Евгения Елпанова
;http://www.autocad.ru/cgi-bin/f1/board.cgi?t=20450SW
;;;http://www.arcada.com.ua/forum/viewtopic.php?t=481&sid=69bf50f6022d526c7c56ad2029d9f24c
(defun lib:plineLW-reverse ( lw / e x1 x2 x3 x4 x5 x6)
  (if (= (type lw) 'VLA-OBJECT)
    (setq lw (vlax-vla-object->ename lw)))
    (setq e (entget lw))
(foreach a1 e 
   (cond 
     ((= (car a1) 10) (setq x2 (cons a1 x2))) 
     ((= (car a1) 40) (setq x4 (cons (cons 41 (cdr a1)) x4))) 
     ((= (car a1) 41) (setq x3 (cons (cons 40 (cdr a1)) x3))) 
     ((= (car a1) 42) (setq x5 (cons (cons 42 (- (cdr a1))) x5))) 
     ((= (car a1) 210) (setq x6 (cons a1 x6))) 
     (t (setq x1 (cons a1 x1))) 
     ) ;_ end of cond 
   )
(entmod (append(reverse x1)(append(apply(function append)
  (apply (function mapcar)(cons 'list (list x2 
         (cdr (reverse (cons (car x3) (reverse x3)))) 
         (cdr (reverse (cons (car x4) (reverse x4)))) 
         (cdr (reverse (cons (car x5) (reverse x5)))) 
         ) ;_ end of list 
        ) ;_ end of cons 
      ) ;_ end of apply 
         ) ;_ end of apply 
       x6 
       ) ;_ end of append 
     ) ;_ end of append 
   ) ;_ end of entmod 
  (entupd lw) 
  ) ;_ end of defun

;|
    Создает LW полилинию, по существующей (e1), заменяя дуговые сегменты, сглаженные FIT или Spline
    прямолинейными (аппроксимация) количеством maxdelta, если оно >0 или через maxdelta ед. рисунка,
    если оно <0, или по предельное отклонение середины хорды и дуги, если maxdelta - положительное число в списке (1),
    или по длине хорды, если maxdelta - отрицательное число в списке (-1),
    Helper ф-ции:
    lwpline-no-arc  - обработка LWPOLYLINE
    polyline-no-arc - обработка POLYLINE
    Arguments:
        e1 - ENAME or VLA-OBJECT сущ. полилинии
        maxdelta - >0 кол-во сегментов аппроксимации
                   <0 через maxdelta ед. рисунка
                   (maxdelta), maxdelta>0  - (10) предельное отклонение середины хорды и дуги
                   (maxdelta), maxdelta<0  - (-10) длина хорды
       del - t=удалять исходную полдилинию nil = нет
       Rad - min радиус дуги или 0 - любой
    Return:
    Vla-Object LWPOLYLINE if added or nil
    
|;
(defun mip-pline-no-arc ( e1 maxdelta del Rad / vla-pl list:pt)
 (defun lwpline-no-arc ( vla-pl / vx blglist n nvx blg LSegm Le par bpt s RestoreBlgList)
  (setq vx (lib:pline-get-verts vla-pl))
 (if (eq (vla-get-closed vla-pl) :vlax-true)
     (setq vx (append vx (list (car vx)))))
  
  (setq blglist (getblg vla-pl) nvx nil)
  (setq n 0)
  (while (nth (1+ n) vx)
    (setq blg (nth n blglist))
    (if (and blg
	     (not (zerop blg))
	     (setq par (lib:pline-get-radii
		       (vlax-curve-getPointAtParam vla-pl n)
		       (vlax-curve-getPointAtParam vla-pl (1+ n))
		       (vla-GetBulge vla-pl n)))
	     Rad
	     (>= par Rad)
	     )
      (progn
	(setq LSegm (- (vlax-curve-getDistAtParam vla-pl (1+ n))(vlax-curve-getDistAtParam vla-pl n)))
	(cond
	  ((and (listp maxdelta)
		(> (setq s (car maxdelta)) 0))
	   (setq le (* 8.0 s par)
		  s  (* s s)
		 bpt (/ 4.0 3.0)
		  s (* s bpt)
		 le (+ le s)
		 le (sqrt le)))
	  ((and (listp maxdelta)
		(< (setq s (car maxdelta)) 0))
	   (setq  s (* 0.5 (abs s))) ;_длина хорды C
	   (if (>= s par)(setq s (* 0.5 par)))
	   (setq  s (- (* par par)(* s s))
		  s (sqrt s)
		  s (- par s)
	       le (* 8.0 s par)
		  s  (* s s)
		 bpt (/ 4.0 3.0)
		  s (* s bpt)
		 le (+ le s)
		 le (sqrt le)))
	  ((> maxdelta 0)(setq le (/ LSegm maxdelta)))
	  ((< maxdelta 0)
	   (setq le (abs maxdelta))
	   (if (> le LSegm)(setq le (/ LSegm 2.0))))
	   (t nil)
	   )
	(setq par (/ le LSegm) blg n)
	(while (< (setq blg (+ blg par)) (1+ n))
	  (setq bpt (vlax-curve-getPointAtParam vla-pl blg))
	  (setq nvx (append nvx (list bpt))));_while
      )
      )
    (setq n (1+ n))
    )
   (if (not del)(setq vla-pl (vla-copy vla-pl)))
   (setq bpt (vlax-vla-object->ename vla-pl))
     (foreach pt nvx (pl:AddVX pt bpt pt))
   (setq blg 0 n (fix (vlax-curve-getEndParam vla-pl)))
  (while (and (vlax-method-applicable-p vla-pl 'Getbulge)
	      (< blg n))
    (if (and (not (zerop(vla-GetBulge vla-pl blg)))
	    (setq par (lib:pline-get-radii
		       (vlax-curve-getPointAtParam vla-pl blg)
		       (vlax-curve-getPointAtParam vla-pl (1+ blg))
		       (vla-GetBulge vla-pl blg)))
		  (>= par Rad))
     (vla-SetBulge vla-pl blg 0.0))
     (setq blg (1+ blg))
    )
    );_defun
  (defun polyline-no-arc ( vla-pl / coors prev_dist Fpt Spt LSegm buf count pt1 int:i pt var ret)
     (setq prev_dist 0)
  (setq coors (LIB:PLINE-GET-VERTS vla-pl))
  (if (> maxdelta 0)
    (setq maxdelta (* maxdelta (- (length coors) 1))))
  (setq Fpt (car coors))
  (setq Spt (last coors))
    (setq prev_dist (vlax-curve-getDistAtPoint vla-pl Fpt))
    (setq LSegm (- (vlax-curve-getDistAtPoint vla-pl Spt) prev_dist)) ;;;Длина сегм
     (cond
       ((listp maxdelta)(setq maxdelta 1))
       ((> maxdelta 0)
	(setq buf (/ LSegm maxdelta))
	(setq count maxDelta))
       ((< maxdelta 0)
        (setq buf (abs maxdelta))
        (if (> buf LSegm)(setq buf (/ LSegm 2.0)))
	(setq pt1 (/ LSegm (abs maxDelta)))
        (setq count (+ (fix pt1) 1))
	)
        (t (setq maxdelta 1))
       )
    (setq int:i 0)
    (repeat (- count 1)
      (setq int:i (1+ int:i))
      (setq pt (vlax-curve-getPointAtDist vla-pl (+ prev_dist (* int:i buf))))
      (setq list:pt (append list:pt (list pt)))
	)
    (setq Fpt Spt)
    (setq list:pt (append (list (car coors)) list:pt (list (last coors))))
   );_defun
    (if (= (type e1) 'ENAME)
      (setq vla-pl (vlax-ename->vla-object e1))
      (setq vla-pl e1 e1 (vlax-vla-object->ename vla-pl))
      )
      (setq ret vla-pl)
      (cond
	((= (vla-get-ObjectName vla-pl) "AcDbPolyline")(lwpline-no-arc vla-pl)(setq list:pt nil))
	((= (vla-get-ObjectName vla-pl) "AcDb2dPolyline")(setq list:pt (polyline-no-arc vla-pl)))
	(t nil))
  (if list:pt
    (progn
      (setq list:pt (mapcar '(lambda ( pt)(list  (car pt)(cadr pt))) list:pt))
      (setq list:pt (apply 'append list:pt))
      (setq var (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray
                  vlax-vbDouble (cons 0 (1- (length list:pt)))) list:pt)))
      (setq ret (vla-addLightWeightPolyline (vla-ObjectIDToObject *pl-activedoc* (vla-get-OwnerID vla-pl)) var))
      (mapcar
	   '(lambda (x y) (vlax-put-property ret x y))
	   '(Linetype LineWeight Color Layer)
	   (mapcar
	     '(lambda (x)
		(vlax-get-property vla-pl  x))
	     '(Linetype LineWeight Color Layer)))
      (if del (vla-Delete vla-pl))
      )
    nil)
  ret
  )


;;;Сохранение полилинии
(defun pltools-savepline ( pl / vx blg)
  (setq vx (lib:pline-get-verts pl) blg (getblg pl)
      *PLTOOLS-UNDOLST* (append (list(list pl (apply 'append vx) blg)) *PLTOOLS-UNDOLST*)))
;;;Восстановление полилинии
;;;Возвращает t - восстановлена nil - нечего восстанавливать  
(defun pltools-restorepline ( / pl vx blg i ret lst nab)
  (if (= (type (car *PLTOOLS-UNDOLST*)) 'INT)
    (progn
    (setq vx (car *PLTOOLS-UNDOLST*))  
    (setq *PLTOOLS-UNDOLST* (cdr *PLTOOLS-UNDOLST*))  
    (repeat vx
      (setq lst (append lst (list (car *PLTOOLS-UNDOLST*))))
      (setq *PLTOOLS-UNDOLST* (cdr *PLTOOLS-UNDOLST*))
      )
    )
    (progn
      (setq lst (list(car *PLTOOLS-UNDOLST*)))
      (setq *PLTOOLS-UNDOLST* (cdr *PLTOOLS-UNDOLST*))
      )
    )
  (foreach vx lst
  (if vx (progn
    (setq pl (car vx) blg (caddr vx) vx (cadr vx))
    (vla-put-coordinates pl (vlax-make-variant (vlax-safearray-fill
      (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length vx)))) vx)))
    (setq i 0)
    (if (vlax-method-applicable-p pl 'Setbulge)
      (mapcar '(lambda (x)(if x (progn (vla-SetBulge pl i x)(setq i (1+ i))))) blg))
    (setq ret t)
    )
    (setq ret nil)
    ))
  (if ret (progn
      (setq nab nil nab (ssadd))
      (ssadd (vlax-vla-object->ename pl) nab)
      (SSSETFIRST nab nab)(setq nab nil)))
  ret)
  
(defun mip_grdraw ( ptdraw ang color / pt1 pt2 )
  (setq pt1 (polar ptdraw (+ ang (pl:DTR 135)) (* 0.05 (getvar "VIEWSIZE"))))
  (setq pt2 (polar ptdraw (+ ang (pl:DTR 225)) (* 0.05 (getvar "VIEWSIZE"))))
  (grvecs (list color pt1 ptdraw ptdraw pt2))
  )
(defun _vxgrdraw ( ptdraw color / ang pt11 pt12 pt21 pt22 len )
  (setq len (* 0.03 (getvar "VIEWSIZE"))
       ang 0
       pt11 (polar ptdraw (+ ang (pl:DTR 225)) len)
       pt12 (polar ptdraw (+ ang (pl:DTR 45)) len)
       pt21 (polar ptdraw (+ ang (pl:DTR 315)) len)
       pt22 (polar ptdraw (+ ang (pl:DTR 135)) len))
  (grvecs (list color pt11 pt12 pt21 pt22))
  )

(defun ru-list-massoc (key alist) 
(mapcar 'cdr (vl-remove-if-not (function (lambda (x) (= key (car x)))) alist))) ;_ end of defun 
 ;_ end of defun 


;;;Реверс полилиний POLYLINE
;;;Взято http://www.arcada.com.ua/forum/viewtopic.php?t=481
(defun ru-geom-polyline-revers 
                               (ent_name        / 
                                _dxf-code-data  tmp_ent 
                                i               poly_ent 
                                vert_ent        vertex_list 
                                bulge_list      start_width_list 
                                end_width_list 
                               )
  (defun _dxf-code-data (code ent_name) 
    (cdr (assoc code (entget ent_name))) 
  ) ;_ end of defun 
  
(if (= (type ent_name) 'VLA-OBJECT)
    (setq ent_name (vlax-vla-object->ename ent_name)))
  ;; Реверс "POLYLINE" 
  (setq tmp_ent ent_name 
        ;; копируем имя выбранного примитива 
        i       0 
  ) ;_ end of setq
  (setq bulge_list nil vertex_list nil start_width_list nil end_width_list nil)
  (while 
    (not (= "SEQEND" (_dxf-code-data 0 (setq tmp_ent (entnext tmp_ent))))) ;_ end of not 
     (setq bulge_list 
            (cons (_dxf-code-data 42 tmp_ent) bulge_list) 
           vertex_list 
            (cons (entget tmp_ent) vertex_list) 
           start_width_list 
            (cons (_dxf-code-data 40 tmp_ent) 
                  start_width_list 
            ) ;_ end of cons 
           end_width_list 
            (cons (_dxf-code-data 41 tmp_ent) end_width_list) 
           i (1+ i) 
     ) ;_ end of setq 
  ) ;_ end of while
  
  (setq
        bulge_list       (append (cdr bulge_list) (list (car bulge_list))) 
        start_width_list (append (cdr start_width_list) 
                                 (list (car start_width_list)) 
                         ) ;_ end of append 
        end_width_list   (append (cdr end_width_list) 
                                 (list (car end_width_list)) 
                         ) ;_ end of append
        i                0
        bulge_list       (mapcar '(lambda (x) (- 0 x)) bulge_list) 
        tmp_ent          ent_name 
        poly_ent         (cdr (entget tmp_ent)) 
        poly_ent         (subst (cons 40 (car end_width_list)) 
                                (assoc 40 poly_ent) 
                                poly_ent 
                         ) ;_ end of subst 
        poly_ent         (subst (cons 41 (last start_width_list)) 
                                (assoc 41 poly_ent) 
                                poly_ent 
                         ) ;_ end of subst 
  ) ;_ end of setq 
  (entmake poly_ent)          ; polyline
  (while 
    (not (= "SEQEND" (_dxf-code-data 0 (setq tmp_ent (entnext tmp_ent)))) 
    ) ;_ end of not 
     (progn 
       (setq vert_ent (nth i vertex_list) 
             vert_ent (subst (cons 40 (nth i end_width_list)) 
                             (assoc 40 vert_ent) 
                             vert_ent 
                      ) ;_ end of subst 
             vert_ent (subst (cons 41 (nth i start_width_list)) 
                             (assoc 41 vert_ent) 
                             vert_ent 
                      ) ;_ end of subst 
             vert_ent (subst (cons 42 (nth i bulge_list)) 
                             (assoc 42 vert_ent) 
                             vert_ent 
                      ) ;_ end of subst 
             i        (1+ i) 
       ) ;_ end of setq
       (entmake vert_ent) 
     ) ;_ end of progn 
  ) ;_ end of while 
  (entmake (cdr (entget tmp_ent))) ; seqend
  (entdel ent_name) 
  (redraw (entlast))
  (entlast)
) ;_ end of defun

;;; Конвертирование объектов в полилинию
(defun conv-ent-to-pline
       (ent / ent_type pl H pt0 cen W var crs csp)
  (or *pl-activedoc*
      (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
      (setq ent_type (cdr (assoc 0 (entget ent)))
	          pl (vlax-ename->vla-object ent)
	         csp (vla-ObjectIDToObject *pl-activedoc* (vla-get-OwnerID pl)))
  (cond	((= ent_type "CIRCLE")
	 (setq pt0 (polar (vlax-safearray->list
				   (vlax-variant-value (vla-get-Center pl)))
				 0 (vla-get-Radius pl))
	       pt0 (vlax-curve-getclosestpointto pl pt0)
	       H (vlax-curve-getParamAtPoint pl pt0)
	       pt0 (trans pt0 0 ent))
	 (setq crs (apply 'append
			  (mapcar '(lambda (x) (vl-remove (last x) x))
				  (list	pt0
					(trans (vlax-curve-getPointAtParam
						 pl
						 (if (<= H pi)(- H pi)(+ H pi))) 0 ent)
					pt0)))
	       cen (last (trans (vlax-get pl 'Center) 0 ent))
	       W   '(1 1))
	 (setq var (vlax-make-variant
		     (vlax-safearray-fill
		       (vlax-make-safearray
			 vlax-vbDouble
			 (cons 0 (1- (length crs))))
		       crs)))
	 (setq pl (vla-addLightWeightPolyline csp var))
	 (mapcar '(lambda (x y) (vla-setBulge pl x y)) '(1 0) W)
	 (vla-put-elevation pl cen)
	 (mapcar
	   '(lambda (x y) (vlax-put-property pl x y))
	   '(Linetype LineWeight Color Layer)
	   (mapcar
	     '(lambda (x)
		(vlax-get-property (vlax-ename->vla-object ent) x))
	     '(Linetype LineWeight Color Layer)))
	 (entdel ent)
	 (setq ent (vlax-vla-object->ename pl))
	)
	((= ent_type "ARC")
	 (if (and (getvar "PEDITACCEPT") (= (getvar "PEDITACCEPT") 1))
	   (vl-cmdf "_.PEDIT" ent "")
	   (vl-cmdf "_.PEDIT" ent "" ""))
	 (setq ent (entlast))
	)
	((= ent_type "LINE")
	 (setq crs (mapcar '(lambda (x) (vlax-get pl x))
			   '(StartPoint EndPoint))
	       crs (mapcar '(lambda (x) (trans x 0 ent)) crs)
	       crs (apply 'append crs))
	 (setq var (vlax-make-variant
		     (vlax-safearray-fill
		       (vlax-make-safearray
			 vlax-vbDouble
			 (cons 0 (1- (length crs)))) crs)))
	 (setq pl (vla-Add3DPoly csp var))
	 (mapcar
	   '(lambda (x y) (vlax-put-property pl x y))
	   '(Linetype LineWeight Color Layer)
	   (mapcar
	     '(lambda (x)
		(vlax-get-property (vlax-ename->vla-object ent) x))
	     '(Linetype LineWeight Color Layer)))
	 (entdel ent)
	 (setq ent (vlax-vla-object->ename pl))
	)
	((= ent_type "VIEWPORT")
	 (setq cen (vlax-get pl 'center)
	       w   (* 0.5 (vla-Get-Width pl))
	       h   (* 0.5 (vla-get-Height pl))
	       crs (mapcar '(lambda (x y)
			      (list (+ (car cen) x) (+ (cadr cen) y)))
			   (list (- 0 w) (- 0 w) w w)(list (- 0 h) h h (- 0 h)))
	       crs (apply 'append crs)
	       var (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble
			 (cons 0 (1- (length crs)))) crs))
	       pl  (vla-addLightWeightPolyline csp var))
	 (vla-put-Closed pl :vlax-true)
	 (vl-cmdf "_.VPCLIP" ent (vlax-vla-object->ename pl))
	 (mapcar
	   '(lambda (x y) (vlax-put-property pl x y))
	   '(Linetype LineWeight Color Layer)
	   (mapcar '(lambda (x)
		      (vlax-get-property (vlax-ename->vla-object ent) x))
		   '(Linetype LineWeight Color Layer)))
	 (setq ent (vlax-vla-object->ename pl)))
	(t nil))
  ent
)


(defun dxf (n ent)(cdr (assoc n (entget ent))))

;;;Функция возвращает список координат ширин и кривизн полилинии
;;; pl-ename or vla object
;;; Возвращается список ввиде 4 списков
;;; 1-й список координат (WCS)
;;; 2-й список начальная ширина 
;;; 3-й список конечная ширина
;;; 4-й список кривизн
(defun pl-get-coors&width&bulge ( pl / ent_data tmp_ent start_width end_width blglist coors)
(setq pl (pl:conv-ent-to-ename PL))  
  (setq ent_data (entget pl))
  (if (= (cdr(assoc 0 ent_data))  "LWPOLYLINE")
    (foreach lst ent_data
      (setq num (car lst))
      (cond
        ((= num 10)(setq coors (cons (cdr lst) coors)))
        ((= num 40)(setq start_width (cons (cdr lst) start_width)))
        ((= num 41)(setq end_width (cons (cdr lst) end_width)))
        ((= num 42)(setq blglist (cons (cdr lst) blglist)))
        (t nil)
        )
      )
    (progn
      (setq tmp_ent pl)
      (while (/= "SEQEND" (cdr(assoc 0 (setq ent_data (entget(setq tmp_ent (entnext tmp_ent)))))))
        
             (setq coors (cons (cdr (assoc 10 ent_data))  coors))
             (setq start_width (cons (cdr (assoc 40 ent_data)) start_width))
             (setq end_width (cons (cdr (assoc 41 ent_data)) end_width))
             (setq blglist (cons (cdr (assoc 42 ent_data)) blglist))
	   );_while
      )
    )
  (list (reverse coors)
        (reverse start_width)
        (reverse end_width)
        (reverse blglist)
        )
  )

(defun pl-set-coors&width&bulge ( pl coors start_width end_width blglist / ent_data tmp_list i)
(setq pl (pl:conv-ent-to-ename PL))  
(setq ent_data (entget pl))
  (cond ((= (cdr(assoc 0 ent_data))  "LWPOLYLINE")
              (setq ent_data (vl-remove-if
                           '(lambda (x)(vl-position (car x) '(40 41 42 10))) ent_data))
               (mapcar '(lambda (crs sw ew blg)
                          (setq tmp_list (vl-list*
                                              (cons 42 blg)
                                              (cons 41 ew)
                                              (cons 40 sw)
                                              (cons 10 (list (car crs)(cadr crs)))
                                              tmp_list
                                              )
                                        )
                          )
                                     coors start_width end_width blglist
                       )
         
              (setq ent_data (append ent_data (reverse tmp_list)))
              ;(mapcar '(lambda (x) (setq ent_data (append ent_data x))) tmp_list)
              (setq ent_data (subst (cons 90 (fix(* 0.25 (length tmp_list)))) (assoc 90 ent_data) ent_data))
              (entmod ent_data)
              (entupd pl)
	 )
        (t  (setq i (cadddr (assoc 10 ent_data))) ;_Z value
            (setq coors (mapcar '(lambda(x / Z)
                                   (setq Z (caddr x))
                                   (if (null Z)(setq Z i))
                                   (list (car x)(cadr x) Z)) coors))
            (setq tmp_list (apply 'append coors))
            (vla-put-coordinates (setq i (pl:conv-ent-to-vla PL))(vlax-make-variant (vlax-safearray-fill
            (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length tmp_list)))) tmp_list)))
          (setq pl (pl:conv-ent-to-ename i))   
          (setq tmp_list pl i 0)
	 (while (/= "SEQEND" (cdr(assoc 0 (setq ent_data (entget(setq tmp_list (entnext tmp_list)))))))
	   (setq ent_data (entget tmp_list))
	   (if (nth i start_width)
	     (setq ent_data (subst (cons 40 (nth i start_width))(assoc 40 ent_data) ent_data)))
	   (if (nth i end_width)
	     (setq ent_data (subst (cons 41 (nth i end_width))(assoc 41 ent_data) ent_data)))
	   (if (nth i blglist)
	     (setq ent_data (subst (cons 42 (nth i blglist))(assoc 42 ent_data) ent_data)))
           (entmod ent_data)(setq i (1+ i))		 
	   );_while
	; (entmake (cdr (entget tmp_list)))
	 ;(entdel ent_name)
	   (entupd pl)
	 ))
  pl)

  
 (defun getblg ( pl / blglist i n ent_data tmp_ent)
   (if (= (type pl) 'VLA-OBJECT)(setq pl (vlax-vla-object->ename pl)))
   (setq ent_data (entget pl))
  (cond ((= (dxf 0 pl) "LWPOLYLINE")
	 (setq  blglist (RU-LIST-MASSOC 42 ent_data)))
	(t (setq tmp_ent pl)
	 (while (/= "SEQEND" (dxf 0 (setq tmp_ent (entnext tmp_ent))))
	   (setq  blglist (append blglist (list (dxf 42 tmp_ent)))));_while
	 )
	 )
   
   blglist)


;;; ============ Библиотечные ф-ции POLYLINE END =============================


;|==============================================================
*    Стандартный обработчик ошибок AutoCAD
==============================================================|;
(defun pltool-err (msg)
  (setq *PLTOOLS-UNDOLST* nil)
  (if (member msg '("console break"      "Function cancelled"
    "Функция отменена"    "quit / exit abort" "выйти прервать"))
    (princ "\nКоманда прервана пользователем")
    (princ
      (strcat "\ERRNO # " (itoa (getvar "ERRNO"))
        ": " msg  "\n" )))
  ;;;Прерываем активную команду  
  (while (> (getvar "CMDACTIVE") 0)(command))  
  (pl-error-restore-sysvar)
  (vla-endundomark *pl-activedoc*)
  (SSSETFIRST)
(if acet-ui-progress-done (acet-ui-progress-done))
  (vl-cmdf "_.redrawall")
  (princ)) ;_ end of defun

;;; ============ Библиотечные ф-ции END ===============================

;;;Реверс объектов
(defun C:ENTREV ( / int:i e1 ed list:pt *error* )
(setq *error* pltool-err)
  (or *pl-activedoc*
       (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
  (vla-endundomark *pl-activedoc*)
  (vla-startundomark *pl-activedoc*)
    (pl-error-save-sysvar
      (list '("osmode")	'("CLAYER") '("QAFLAGS" 0) '("CMDECHO" 0)))
  (if *pl:IsRus*
  (princ "\nВыберите Полилинии, Сплайны и Отрезки для реверса  ")
  (princ "\nChoose Polylines, Splines and Lines for a reverser  ")
    )
  (setq PICK1 nil
	PICK1 (ssget "_:L" '((0 . "*LINE"))))
  (setq int:i 0)
  (while (and PICK1 (setq e1 (ssname PICK1 int:i)))
    (setq list:pt nil)
    (setq ed (entget e1))
    (cond ((= (cdr(assoc 0 ed)) "LINE")
	   (setq e1 (vlax-ename->vla-object e1))
	   (setq list:pt (mapcar '(lambda (x) (vlax-get e1 x))
			   '(StartPoint EndPoint))
		 list:pt  (reverse list:pt))
	   (vla-put-StartPoint e1 (vlax-3d-point (car list:pt)))
	   (vla-put-EndPoint e1 (vlax-3d-point (cadr list:pt)))
	  )
	  ((= (cdr(assoc 0 ed)) "LWPOLYLINE")
	   (lib:plineLW-reverse e1)
          ;(entdel e1)
	   )
	  ((= (cdr(assoc 0 ed)) "SPLINE")(vla-reverse (vlax-ename->vla-object e1))) 
  	  ((= (cdr(assoc 0 ed)) "POLYLINE")
	   (ru-geom-polyline-revers e1))
	  (t nil)
    )	  
    (setq int:i (1+ int:i))
  )
   (if *pl:IsRus*
        (princ (strcat "\nВыполнен реверс " (itoa int:i) " объектов"))
        (princ (strcat "\nThe reverser of " (itoa int:i) " object is executed")))
  (setq PICK1 nil)
(pl-error-restore-sysvar)
(vla-endundomark *pl-activedoc*)
  (princ))
;;;Поштучный реверс
(defun C:ENTREVS ( / pt  e1 ed list:pt *error* pl ask par ptn ep)
(setq *error* pltool-err flg t)
  (or *pl-activedoc*
       (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
  (vla-endundomark *pl-activedoc*)
  (vla-startundomark *pl-activedoc*)
    (pl-error-save-sysvar
      (list '("osmode")	'("CLAYER") '("QAFLAGS" 0) '("CMDECHO" 0)))
  (while (setq e1 (entsel (if *pl:IsRus*
                            "\nВыберите Полилинию, Сплайн или Отрезок для реверса <хватит>:"
                            "\nChoose Polyline, Spline or Lines for a reverse <exit>  "
                            )
                          )
               )
    
    (if (and  (setq pt (cadr e1))
	   ;   (setq pt (osnap pt "_nea"))
	      (setq e1 (car e1))
	      (setq pl (vlax-ename->vla-object e1))
	      (setq pt (vlax-curve-getClosestPointTo pl (trans pt 1 0)))
	      (setq par (vlax-curve-getParamAtPoint pl pt))
              (setq par (+ (fix par) 0.5))
              (setq pt (trans (vlax-curve-getPointAtParam pl (+ (fix par) 0.5)) 0 1))
	      (setq ptn (trans (vlax-curve-getPointAtParam pl 0) 0 1))
	      (setq ed (entget e1))
	      (wcmatch (cdr(assoc 0 ed)) "*LINE"))
     (if (pl:Is-object-modifed pl nil)
     (progn
;       (setq pt (trans pt 0 1))
       (mip_grdraw pt
	 (angle pt
           (trans (vlax-curve-getPointAtParam pl (+ par 0.001)) 0 1)) 1)
       (_vxgrdraw ptn -1)
       (initget "Yes Да Н No Нет Т _Yes Yes Yes No No No")
       (setq ask (getkword (if *pl:IsRus* "\nРеверсировать объект? [да Y/ нет N] <Нет>: "
                                          "\nTo execute a reverser? [Yes/No] <No>: ")
                             ))
       (if (= ask "Yes")(setq ed (cdr (assoc 0 ed)))(setq ed ""))
       (if (wcmatch ed "*POLYLINE")(setq ep (vlax-curve-getEndParam (vlax-ename->vla-object e1))))
       (cond ((= ed "LINE")
	      (setq e1 (vlax-ename->vla-object e1))
	      (setq list:pt (mapcar '(lambda (x) (vlax-get e1 x))
				    '(StartPoint EndPoint)
			    )
		    list:pt (reverse list:pt)
	      )
	      (vla-put-StartPoint e1 (vlax-3d-point (car list:pt)))
	      (vla-put-EndPoint e1 (vlax-3d-point (cadr list:pt)))
	     )
	     ((= ed "LWPOLYLINE")(setq e1 (lib:plineLW-reverse e1)))
	     ((= ed "SPLINE") (vla-reverse (vlax-ename->vla-object e1)))
	     ((= ed "POLYLINE")(setq e1 (ru-geom-polyline-revers e1)))
	     (t nil))
       (if (and ed (wcmatch ed "*POLYLINE")
		(= :vlax-true (vla-get-Closed (setq pl (vlax-ename->vla-object e1)))))
	 (progn
	   (pl-vx1 pl (1- ep))
	   )
	 )
      (if *pl:IsRus*
        (princ "\nВыполнен реверс объекта")
        (princ "\nThe reverser of object is executed"))
     )
      (if *pl:IsRus* (princ "*** Объект на блокированном слое ***")
                     (princ "*** Object on locked layer ***")
        )
       )
    (if *pl:IsRus* (princ "*** Объект не Полилиния, Сплайн или Отрезок ***")
                   (princ "*** Please select Polyline, Spline or Line ***"))
   )
   (vl-cmdf "_.redrawall") 
  );_while
(pl-error-restore-sysvar)(vla-endundomark *pl-activedoc*)(redraw)(princ))

;**************** C:PL-L2A *************************************
;                Замена линейного сегмента в полилинии
;                дуговым сегментом.
;                Автор  Евгений Елпанов.
;http://www.autocad.ru/cgi-bin/f1/board.cgi?t=23073xg

(defun C:PL-L2A	(/ A1 ENT GR I LST LW PAR PT *error* rad next c blg next1)
  (defun *error* (msg)
    (setq *PLTOOLS-UNDOLST* (list (car *PLTOOLS-UNDOLST*)))
    (pltools-restorepline)(setq *PLTOOLS-UNDOLST* nil) (SSSETFIRST)
    (vla-endundomark *pl-activedoc*)(vl-cmdf "_.redrawall")(princ))
(setq *PLTOOLS-UNDOLST* nil next1 t)
(or *pl-activedoc* (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
(vla-startundomark *pl-activedoc*)
(while next1 (redraw)
 (setq rad nil) 
 (initget "Отмени Undo J Г Выход D eXit Ч Радиус H R _U U U U X X X X R R R")
 (setq lw(entsel"\n Выберите нужный линейный сегмент в полилинии [отмени U/радиус R/выход X] <выход>:"))
  (if (= lw "R")
    (progn
      (initget 1)
      (setq rad (getreal "\nРадиус дугового сегмента: "))
      (setq lw (entsel "\n Выберите нужный сегмент в полилинии."))))
   (cond
   ((= lw "U")(if (not (pltools-restorepline))(alert "Нечего отменять")))
   ((= lw "X")(setq next1 nil))
   ((null lw)(if (= (getvar "ERRNO") 7)(alert "Ничего не выбрано")(setq next1 nil)))

  ((and lw (= (cdr (assoc 0 (entget (car lw)))) "LWPOLYLINE"))
      (pltools-savepline (vlax-ename->vla-object (car lw))) 
      (setq par	(vlax-curve-getParamAtPoint
		  (car lw)
		  (vlax-curve-getClosestPointTo (car lw)(trans (cadr lw) 1 0))
		)
	    a1	(angle (vlax-curve-getPointAtParam (car lw) (fix par))
		       (vlax-curve-getPointAtParam (car lw) (1+ (fix par)))
		)
	    c  (* 0.5 (distance (vlax-curve-getPointAtParam (car lw) (fix par))
		       (vlax-curve-getPointAtParam (car lw) (1+ (fix par)))))
      )
      (if RAD
	(progn
	  (if (>= (- (* Rad Rad)(* c c)) 0)
	    (setq blg (- (* Rad Rad)(* c c))
		  blg (sqrt blg)
		  blg (- Rad blg)
		  blg (/ blg c))
	    (progn
	      (setq blg (cdr(assoc 42(entget (car lw)))))
	      (alert (strcat "** Недопустимый радиус **\nМинимум=" (rtos c 2 3)))
	    )
	    )
	)
	(setq blg 0))

      (if (not RAD) (princ "\n Задайте визуально кривизну сегмента. "))
      (setq next t)
      (while (and next
		  (or (numberp Rad)
		    (and (setq gr (grread 5)) (= (car gr) 5))))
	(setq i	  0
	      lst nil
	      ent (entget (car lw))
	      gr (cadr gr)
	)
	(while (or (/= (caar ent) 42)
		   (if (< i (fix par))
		     (setq i (1+ i))
		   )
	       )
	  (setq	lst (cons (car ent) lst)
		ent (cdr ent)
	  )
	)
	(redraw)
	(if (not Rad)
	(grdraw	(setq pt (trans (vlax-curve-getPointAtParam
			   (car lw)
			   (fix par)
			 ) 0 1)
		)
		gr
		6
		1
	))
	(entmod
	  (append
	    (reverse
	      (cons (if Rad
		      (cons 42 blg)
		      (cons 42
			  (/ (sin (/ (- a1 (angle pt gr)) 2.))
			     (cos (/ (- a1 (angle pt gr)) 2.))
			  )
		    ))
		    lst
	      )
	    )
	    (cdr ent)
	  )
	)
	(entupd (car lw))
       (if (numberp Rad)(setq next nil))	
      )
   )
   (t (alert "\n Ничего не выбрано или объект не LW полилиния."))
  );_cond
  );_while next1
  (SSSETFIRST)(setq *PLTOOLS-UNDOLST* nil)
  (vla-endundomark *pl-activedoc*)(redraw)(princ))
;                Замена дугового сегмента в полилинии
;                линейным сегментом.
;                Автор  Евгений Елпанов.
;http://www.autocad.ru/cgi-bin/f1/board.cgi?t=23073xg

(defun C:PL-A2L (/ lw *error* next)
(setq *error* pltool-err *PLTOOLS-UNDOLST* nil next t)
  (or *pl-activedoc* (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
  (vla-StartUndoMark *pl-activedoc*)
(while next
 (initget "Отмени Undo J Г Выход D eXit Ч _U U U U X X X X")
 (setq lw(entsel"\n Выберите нужный дуговой сегмент в полилинии [отмени U/выход X] <выход>:"))
 (cond
   ((= lw "U")(if (not (pltools-restorepline))(alert "Нечего отменять")))
   ((= lw "X")(if (= (getvar "ERRNO") 7)(alert "Ничего не выбрано")(setq next nil)))
   ((null lw)(setq next nil))
   ((and lw(=(cdr(assoc 0(entget(car lw))))"LWPOLYLINE"))
    (pltools-savepline (vlax-ename->vla-object (car lw)))
    (vla-SetBulge(vlax-ename->vla-object(car lw))(fix(vlax-curve-getParamAtPoint(car lw)
    (vlax-curve-getClosestPointTo(car lw)(trans (cadr lw) 1 0))))0.))
   (t (alert "\n Ничего не выбрано или объект не LW полилиния."))
  );_cond
  )
  (SSSETFIRST)(setq *PLTOOLS-UNDOLST* nil)
 (vla-EndUndoMark *pl-activedoc*)(princ))
;;Объединение 3d полилидиний ssnab - набор fuzz - точность filter - имена объектов
(defun 3dpljn ( ssnab fuzz filter / ipl cpl pl-list n tmp tmp-list count buf)
(defun make3dpoly ( owner point-list / pl csp var)
  (setq csp (vla-ObjectIDToObject (vla-get-activedocument (vlax-get-acad-object))(vla-get-OwnerID owner)))
  (setq point-list (pl:MakeUniqueMembersOfList point-list))
  (setq point-list (apply 'append point-list))
	 (setq var (vlax-make-variant
		     (vlax-safearray-fill
		       (vlax-make-safearray
			 vlax-vbDouble
			 (cons 0 (1- (length point-list)))) point-list)))
	 (setq pl (vla-Add3DPoly csp var))
	 (mapcar
	   '(lambda (x y) (vlax-put-property pl x y))
	   '(Linetype LineWeight Color Layer)
	   (mapcar
	     '(lambda (x)
		(vlax-get-property owner x))
	     '(Linetype LineWeight Color Layer)))
pl)
(defun prep-lst ()(setq pl-list (subst nil ipl pl-list) pl-list (subst nil cpl pl-list))
 (ssdel (vlax-vla-object->ename cpl) ssnab)(ssdel (vlax-vla-object->ename ipl) ssnab)
  (vla-erase cpl)(vla-erase ipl)(setq cpl tmp))
;  (setq ssnab (ssget))
  (setq pl-list (vl-remove-if-not '(lambda(x)
				     (and x
					  (not (vlax-erased-p x))
					  (pl:Is-object-modifed x nil)
					  (wcmatch (vla-get-Objectname x) filter)
					  ))
		  (pl:selset-to-vlalist ssnab)
		  )
	)
  (setq pl-list (vl-remove-if '(lambda(x)
				 (if (and (vlax-property-available-p x 'Closed)
					     (= (vla-get-closed x) :vlax-true)
					     )
				   t nil)) pl-list))
  (setq n 0 buf pl-list)
  (while (< n (length pl-list))
    (setq cpl (nth n pl-list))
    (setq tmp-list (vl-remove cpl pl-list))
    (setq tmp nil)
    (if (and cpl (wcmatch (vla-get-Objectname  cpl) "AcDb3dPolyline"))
    (ssdel (vlax-vla-object->ename cpl) ssnab))
    (foreach ipl tmp-list
      (if (and cpl ipl (not (vlax-erased-p ipl)))
	(progn
	  (cond
	    ((equal (vlax-curve-getstartpoint cpl)(vlax-curve-getstartpoint ipl) fuzz)
	     (if (setq tmp (make3dpoly cpl (append (reverse (lib:pline-get-verts ipl))(lib:pline-get-verts cpl))))
	       (prep-lst)
	       )
	    )
	    ((equal (vlax-curve-getstartpoint cpl)(vlax-curve-getendpoint ipl) fuzz)
	     (if (setq tmp (make3dpoly cpl (append (lib:pline-get-verts ipl)(lib:pline-get-verts cpl))))
	       (prep-lst)
	       )
	     )
	    ((equal (vlax-curve-getendpoint cpl)(vlax-curve-getstartpoint ipl) fuzz)
	     (if (setq tmp (make3dpoly cpl (append (lib:pline-get-verts cpl)(lib:pline-get-verts ipl))))
	       (prep-lst)
	       )
	    )
	    ((equal (vlax-curve-getendpoint cpl)(vlax-curve-getendpoint ipl) fuzz)
	     (if (setq tmp (make3dpoly cpl (append (lib:pline-get-verts cpl)(reverse(lib:pline-get-verts ipl)))))
	       (prep-lst)
	       )
	    )
	    (t nil)
	  );_cond
	  );_progn
	);_if
      );_foreach
 (if tmp (setq pl-list (append pl-list (list cpl))))

  (setq n (1+ n))
  )
  
  (setq pl-list (vl-remove-if-not '(lambda(x)(and x (not (vlax-erased-p x)))) pl-list) count 0)
  (foreach pl pl-list
    (if (member pl buf)
      nil
      (setq count (1+ count))))
  (if (> count 0)
  (if (= (getvar "SysCodePage") "ANSI_1251")
    (princ (strcat "\nСоздано " (itoa count) " 3М полилиний"))
    (princ (strcat "\nСreated "(itoa count)" 3d polylines"))
    ))
  ssnab
  )
(defun C:PL-JOIN ( / ssnab count en *error*)
  (setq *error* pltool-err)
  (or *pl-activedoc* (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
  (vla-StartUndoMark *pl-activedoc*)
  (pl-error-save-sysvar '(("PEDITACCEPT" 1)("CMDECHO" 0)))
  (setq ssnab (ssget "_I"))
  (while (not ssnab)(setq ssnab (ssget)))
  ;;;И 3d полилинии
  (setq ssnab(3dpljn ssnab 1e-6 "AcDb3dPolyline"))
  (setq en (entlast) count 0)
  (if (> (sslength ssnab) 1)
  (if (and (getvar "PEDITACCEPT") (= (getvar "PEDITACCEPT") 1))
    (vl-cmdf "_pedit" "_Multiple" ssnab "" "_Join" 0 "")
    (vl-cmdf "_pedit" "_Multiple" ssnab "" "_Y" "_Join" 0 ""))
  )
  (while (and (setq en (entnext en))
	      (= (cdr(assoc 0 (entget en))) "LWPOLYLINE"))
    (setq count (1+ count))
    )
(if (> count 0)
(if (= (getvar "SysCodePage") "ANSI_1251")
    (princ (strcat "\nСоздано " (itoa count) " простых полилиний"))
    (princ (strcat "\nСreated "(itoa count)" LW polylines"))
    ))    
  (setq ssnab nil)
  (pl-error-restore-sysvar)
  (vla-EndUndoMark *pl-activedoc*)
  (princ))
(defun C:PL-JOIN3D ( / ssnab *error* )
  (setq *error* pltool-err)
  (or *pl-activedoc* (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
  (vla-StartUndoMark *pl-activedoc*)
  (pl-error-save-sysvar '(("CMDECHO" 0)))
  (setvar "CMDECHO" 0)
  (setq ssnab (ssget "_I" '((0 . "POLYLINE,LINE"))))
  (while (not ssnab)(setq ssnab (ssget '((0 . "POLYLINE,LINE")))))
  ;;;И 3d полилинии
  (3dpljn ssnab 1e-6 "AcDb3dPolyline,AcDbLine")
  (setq ssnab nil)(sssetfirst)
  (pl-error-restore-sysvar)
  (vla-EndUndoMark *pl-activedoc*)
  (princ))
;|*********************** Команда PL-DIV *********************************
*                                                                        *
*   Разбивает выбранный сегмент полилинии на указанное количество        *
*  сегментов или через указанное расстояние. По аналогии с командами     *
*  подели (divide) и разметь (measure). Если разбиваем сегмент через     *
*  указанное расстояние, то новые вершины добавляются начиная с вершины, *
*  ближайшей к точке указания.                                           *
*  В ответ на запрос "[Длина (-) /Количество (+) ] сегментов : "         *
*  можно вводить                                                         *
*   - число >0 - воспримется как кол-во сегментов                        *
*   - число <0 - воспримется как расстояние для разметки                 *
*   - соответствующие опции команды (Д К + -)                            *
*                                                                        *
**************************************************************************
Идея и первая реализация здесь http://www.autocad.ru/cgi-bin/f1/board.cgi?t=27884PF
Обсуждения и варианты здесь http://www.autocad.ru/cgi-bin/f1/board.cgi?t=20156yO
   Автор  Владимир Азарко (VVA)  Редакция 21.06.2006|;
(defun C:PL-DIV ()(PL:DIV nil))
(defun C:PL-DIVALL () (PL:DIV t))
;;; all - t все сегменты nil - выбранный
(defun PL:DIV ( all / ent-pline break-point-count osm  *error* next nab pln i n slst many)
(defun vis_segment ( ent / pt pln p1 p2)
  (VL-CATCH-ALL-APPLY '(lambda()
  (setq pt (cadr ent) pln (vlax-ename->vla-object (car ent)))
  (setq	pt (trans pt 1 0))
  (setq	pt (vlax-curve-getclosestpointto pln pt))
  (if (= (vla-get-ObjectName pln) "AcDb3dPolyline")
    (progn
      (setq p1 (last (mip-pline-get-segcoors-by-geom pln pt)))
      (setq p2 (1+ (setq p1 (fix p1))))
      )
    (progn
      (setq p1 (vlax-curve-getParamAtPoint pln pt))
      (setq p2 (1+ (setq p1 (fix p1))))
      )
    )
  (_vxgrdraw (trans (vlax-curve-getPointAtParam pln p1) 0 1) 1)
  (_vxgrdraw (trans (vlax-curve-getPointAtParam pln p2) 0 1) 1)
                         )
    )
  )
(setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))
       next t)
(setq *error* pltool-err)
(pl-error-save-sysvar '(("ANGBASE" 0)("ANGDIR" 0)("UCSFOLLOW" 0)
			     ("UCSICON") ("CLAYER")("osmode" 0)("CMDECHO" 0)))
 (setq *PLTOOLS-UNDOLST* nil)  
(vla-startundomark *pl-activedoc*)
(while next (setvar "ERRNO" 0)(vl-cmdf "_redrawall")
  (initget "Отмени Undo J Г Выход D eXit Ч _U U U U X X X X")
  (setq ent-pline 
   (entsel "\nВыберите сегмент полилинии для добавления вершин [отмени U/выход X] <выход>: "))
    (cond
      ((= ent-pline "U")
       (if many (progn
	 (repeat (car many)(pltools-restorepline))
	 (setq many (cdr many)))
       (if (not (pltools-restorepline))(alert "Нечего отменять"))))
      ((= ent-pline "X")(setq next nil))
      ((null ent-pline)(if (= (getvar "ERRNO") 7)(alert "Ничего не выбрано")(setq next nil)))
      ((and ent-pline (listp ent-pline)(pl:Is-object-modifed (car ent-pline) nil)
        (member (vla-get-ObjectName
                  (vlax-ename->vla-object (car ent-pline)))
                '("AcDb2dPolyline" "AcDbPolyline" "AcDb3dPolyline")))
      (pl:VxOpt (car ent-pline))
      (setvar "CLAYER" (cdr(assoc 8 (entget (car ent-pline)))))
      (setq nab nil nab (ssadd))
      (ssadd (car ent-pline) nab)
      (SSSETFIRST nab nab)(setq nab nil)(vis_segment ent-pline)
 ;;;      (redraw (car ent-pline) 3)
      (initget "Количество Длина + -" 3)
      (prompt "\nЧисло - -длина сегмента + -количество сегментов")
      (setq break-point-count
             (getint "\n[Длина (-) /Количество (+) ] сегментов : "))
      (cond
	((and (= (type break-point-count) 'STR)
              (member (substr break-point-count 1 1) '("Д" "-")))
	 (initget 7)
	 (setq break-point-count (getdist "\nУкажите длину сегментов : ")
	       break-point-count (- 0 break-point-count)))
	((and (= (type break-point-count) 'STR)
	      (member (substr break-point-count 1 1) '("К" "+")))
              (initget 7)
              (setq break-point-count(getint "\nУкажите количество сегментов : ")))
             (t nil));_cond
;;;      (redraw (car ent-pline) 4)
       (setvar "OSMODE" 0)
       (pltools-savepline (pl:conv-ent-to-vla (car ent-pline)))
       (if all
         (pline-addvertex-divide (car ent-pline) break-point-count)
       (progn
         (setq pln (pl:conv-ent-to-vla (car ent-pline))
 	      i (cadr ent-pline) i (trans i 1 0)
              i (vlax-curve-getclosestpointto pln i)
	      i (vlax-curve-getparamatpoint pln i)
	      i (fix i)
	    )
         (pline-addvertex-divide (list (car ent-pline) i) break-point-count)
         )
       )
     )
     (t (alert "Объект не полилиния\nИли слой блокирован"))
      )
  );_while
  (vl-cmdf "_redrawall");(setq *PLTOOLS-UNDOLST* nil)
  (pl-error-restore-sysvar)
  (SSSETFIRST)(vla-endundomark *pl-activedoc*)(princ))



;;;Вычисляет точку путем поворота точки pt1 на угол alf (в радианах) вокруг 0,0
(defun pl-rotate_0_a ( pt1 alf / Ro fi x1 y1)
  (setq Ro (distance '(0 0 0) pt1))
  (setq fi (angle '(0 0 0) pt1))
  (setq x1 (* Ro (cos (+ alf fi))))
  (setq y1 (* Ro (sin (+ alf fi))))
  (list x1 y1)
)

;;;; Точка лежащая от точки P1 на расстоянии L в направлении точки P2
;;;;-----------------------------------------------------------------
;;;;Параметры:
;;;;p1 - первая точка
;;;;p2 - точка в направлении которой производится откладка
;;;;L  - расстояние
;;;; Point laying from point P1 on distance L in a direction of point P2
;;;; Arguments:
;;;;p1 - first point (point from)
;;;;p2 - second point (point to)
;;;;L  - distance

(defun pl-geom-polar-3d (P1 P2 L)
 (setq L (/ L (distance P1 P2)))
 (mapcar '+ P1 (mapcar '(lambda (V) (* V L)) (mapcar '- P2 P1)))
)


;| Функция pline-addvertex-divide
Добавляет вершины к сегменту полилинии
Параметры:
   ent_pline          - ссылка на полилинию, аналог (entsel) или entlast
                        если 1 сегмент точка (cadr ent_pline) - номер сегмента
                        или ename если по всем сегментам
   break-point-count  - число >1 кол-во сегментов (divide)
                      - число <0 длина сегмента   (measure)|;
;;;(setq ent-pline (car(entsel)) break-point-count 2)
;;;(setq s_seg (nth 0 vx) e_seg (nth 1 vx))
;;;(pline-addvertex-divide (car(entsel)) 3)
(defun pline-addvertex-divide (ent-pline break-point-count /
			       pln bpt blg vx s_width e_width)
  (if (listp ent-pline)(setq bpt (cadr ent-pline)))
  (setq pln (pl:conv-ent-to-vla ent-pline))
 (setq blg (pl-get-coors&width&bulge pln)
       vx (nth 0 blg)
       s_width (nth 1  blg)
       e_width   (nth 2 blg)
       blg (nth 3 blg)
       )
    (if (eq (vla-get-closed pln) :vlax-true)
    (setq vx (append vx (list (car vx)))))
  (setq n '-1 nvx nil n_s_width nil n_e_width nil n_blg nil)

  (mapcar '(lambda ( s_seg e_seg / pt1 ang delta sw ew slen i sw1 ew1 ptc ang count what cblg)
             (setq n (1+ n)
                   cblg (nth n blg)
                   what (if (minusp cblg) - +)
                   )
;(setq  s_seg (nth n vx) e_seg (nth (1+ n) vx))             
(grtext -1 (strcat "Обрабатываю сегмент N " (rtos n 2 0)))
             
(if (or (null bpt)(equal bpt n 1e-6)) ;;; bpt - null - все сегменты
                                      ;;; bpt = № начальной вершины
(progn
  (if (not (equal s_seg(car nvx) 1e-6))
               (setq nvx (cons s_seg nvx)
                     n_blg (cons cblg n_blg)
                     )
               )
  
;;;Высчитываем приращение delta, для дуги - угол в радианах.
;;; И количество count
             (if (equal cblg 0.0 1e-6)
                   (progn ;_Линейный
                     (setq ang (angle s_seg e_seg)
                           slen (distance s_seg e_seg)
                           )
                     (if (minusp break-point-count)
                       (setq delta (abs break-point-count)
                             count (1+ (fix (/ slen delta)))
                             )
                       (setq count break-point-count
                             delta (/ slen count)
                             )
                       )
                     )
                  (progn ;_Угловой
                     (setq ptc (BULGECENTERRADIUS cblg s_seg e_seg))
                     (setq ang (abs(* (atan cblg) 4)))
                     (setq slen (* ang (car ptc))) ;_Длина дуги
                     (if (minusp break-point-count)
                       (setq delta (abs break-point-count)
                             count (/ slen delta)
                             delta (if (< count 1) ang (/ ang count))
                             count (1+ (fix count))
                             )
                       (setq count break-point-count
                             delta (/ ang count)
                             )
                       )
                    ;;;Знак угла поворота
                    ;;;Если кривизна положительна (+) то обход против часовой стрелки
                    ;;;Если кривизна положительна (-) то обход по часовой стрелке
                    (setq delta (what 0 delta))
                    ;(setq delta (if (minusp (caddr (pl:3d_Wnorm s_seg e_seg))) (- 0 delta) delta))
                    )
               )
               (setq pt1 s_seg
                     sw (nth n s_width)
                     ew (nth n e_width)
                     )
             (setq n_s_width (cons sw n_s_width))
             (setq i 0 )
             (repeat (1- count)
               (if (equal cblg 0.0 1e-6)
                 (progn ;_Линейный
                   (setq pt1 (pl-geom-polar-3d pt1 e_seg delta))
                   ;(setq pt1 (polar pt1 ang delta))
                   (setq nvx (cons pt1 nvx))
                   (setq n_blg (cons 0.0 n_blg))
                   (setq ew1 (+ sw (/ (* (- ew sw)(setq i (1+ i)) (abs delta)) slen)))
                   (setq n_e_width (cons ew1 n_e_width))
                   (setq n_s_width (cons ew1 n_s_width))
                   )
                 (progn ;_Угловой
                   (setq i (1+ i))
                   (if (equal pt1 e_seg 1e-6)
                     (setq cblg (if (nth (1+ n) blg)(nth (1+ n) blg) 0))
                     (setq pt1 (pl-rotate_0_a
                                 (mapcar '- pt1 (cadr ptc))
                                 delta
                                 )
                           pt1 (mapcar '+ pt1 (cadr ptc))
                           ;cblg (- ang (* i (abs delta)))
                           cblg (abs (tan (* 0.25 delta)))
                           cblg (what 0 cblg)
                           )
                     )
                   (setq nvx (cons pt1 nvx))
                   (setq ew1 (+ sw (/ (* (- ew sw) i (abs delta)) ang)))
                   (setq n_e_width (cons ew1 n_e_width))
                   (setq n_s_width (cons ew1 n_s_width))
                   ;;;Заменяем предыдущую кривизну
                   (setq n_blg (cons cblg (cdr n_blg)))
                   ;;;Текущую такую же
                   (setq n_blg (cons cblg n_blg))
                   )
                 )
               );_repeat
                     (setq n_e_width (cons ew n_e_width))
                     
                     (if (not (equal e_seg (car nvx) 1e-6))
                       (progn
                         (if (equal (nth n blg) 0.0 1e-6)(setq cblg 0.0)
                           (progn
                             ;;;Не правиль высчитывается кривизна см pl-vxadd
                             (setq cblg (- ang (* (1- count)(abs delta)))) ;_Остаток угла
                             (setq cblg (abs (tan(* 0.25 (abs cblg))))
                                   cblg (what 0 cblg)
                                   n_blg (cons cblg (cdr n_blg))
                                   )
                             )
                           )
                         (setq cblg (if (nth (1+ n) blg)(nth (1+ n) blg) 0.0))
                         (setq nvx (cons e_seg nvx)
                               n_blg (cons cblg n_blg)
                               )
                         )
                       )
  )
  (progn
   (if (not (equal s_seg(car nvx) 1e-6)) 
    (setq nvx (cons s_seg nvx)
          n_blg (cons cblg n_blg)
          n_e_width (cons (nth n e_width) n_e_width)
          n_s_width (cons (nth n s_width) n_s_width)
          )
     )
    )
  )
             )
          vx (cdr vx)
          )
   (if (not (equal (last vx)(car nvx) 1e-6))
     (setq nvx (cons (last vx) nvx)
          n_blg (cons (last blg) n_blg)
          n_e_width (cons (last e_width) n_e_width)
          n_s_width (cons (last s_width) n_s_width)
          )
     )
  (setq nvx (reverse nvx)
        n_s_width (reverse (cons (car n_e_width) n_s_width));_Добавляем в последнюю вершину толщину вначале = в конце
        n_e_width (reverse (cons (car n_e_width) n_e_width));_Добавляем в последнюю вершину толщину вначале = в конце
        n_blg (reverse n_blg)
        )
;(setq pln! pln nvx! nvx sw! n_s_width ew! n_e_width blg! n_blg)(getstring "AAA")
      (PL-SET-COORS&WIDTH&BULGE pln nvx n_s_width n_e_width n_blg)
  );_pline-addvertex-divide
  

;| ! ****************************************************************************
   !               pl:AddVX
   ! ****************************************************************************
   ! Function : Add a new point on the polyline in the specified point
   !            
   ! Arguments: 'Pt0'     - selected point POLYLINE in WCS (Точка выбора примитива в  МСК).
                'lw0'  - The name of the POLYLINE entity (Ename) Имя примитива (ENAME)
   !            'Pt'     - New value of the point to be added in WCS. Новая точка в МСК
   ! Action   : Adds a vertex at the specified point along the polyline
   ! Returns  : Vla-object 
   ! Начало положил Елпанов Евгений, огранку произвел Лентяй, доработал VVA
   http://dwg.ru/forum/viewtopic.php?t=8509&start=15&sid=10d0975b300a1b77fa37adfe48f50cfe
   ! ****************************************************************************|;


(defun mip-pline-get-segcoors-by-geom ( crv pt0 / vx p1 p2 par)
(setq pt0 (vlax-curve-getclosestpointto crv pt0))
(setq vx ((lambda ( / i lst)(while (<= (setq i (if i (1+ i) 0))(vlax-curve-getEndParam crv))
                       (setq lst (append lst (list (vlax-curve-getPointAtParam crv i))))) lst)))
(setq p1 (car vx) vx (cdr vx) par 0)
(while (and (setq p2 (car vx))
		 (not (equal (+ (distance Pt0 P1) (distance Pt0 P2)) (distance P1 P2) 0.00001))
			 )
	       (setq vx (cdr vx) p1 p2 par (1+ par))
 )
(list p1 p2 (+ par (/ (distance p1 pt0)(distance p1 p2))))
  )

(defun pl:AddVX ( pt0  lw0 pt / ang ans blg bwl cpt crs csp ew i j lst lw par pts ret sw var wl)
  (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))
  	lw  (vlax-ename->vla-object lw0)
	pt0 (vlax-curve-getclosestpointto lw pt0) ;_Точка указазания в WCS
	pt  (trans pt 0 lw0)                      ;_Новая точка в OCS
	csp (vla-ObjectIDToObject *pl-activedoc* (vla-get-OwnerID lw))
	par (vlax-curve-getParamAtPoint lw pt0)
	ret lw)
  (cond	((member (vla-get-ObjectName lw) '("AcDbPolyline" "AcDb2dPolyline"))
           (setq crs (vlax-get lw 'Coordinates) i 0)
           (while (<= i (fix par))
             (if (= (vla-get-ObjectName lw) "AcDbPolyline")
               (setq lst (reverse (cons (cadr crs) (cons (car crs) (reverse lst))))
                     crs (cddr crs))
               (setq lst (reverse (cons (caddr crs) (cons (cadr crs) (cons (car crs) (reverse lst)))))
                     crs (cdddr crs)));if
             (setq  i (1+ i)));while
           (setq crs (append lst (if (= (vla-get-ObjectName lw) "AcDbPolyline")
                                   (vl-remove (last pt) pt) pt) crs) j i)
           (if (or (not (vlax-property-available-p lw 'type))
                   (and (vlax-property-available-p lw 'type) (= (vla-get-type lw) 0))) (progn
               (setq blg (vla-getBulge lw (1- i))
                     pts (mapcar '(lambda (x) (trans (vlax-curve-getPointAtParam lw x) 0 1)) (list (1- i) i))
                     ang (/ (- pi (* 4 (atan blg))) 2)
                     cpt (polar (car pts) (+ (apply 'angle pts) ang) (* 0.5 (/ (apply 'distance pts) (cos ang))))
                     ans (mapcar '(lambda (x) (vla-AngleFromXAxis (vla-get-utility *pl-activedoc*) (vlax-3d-point cpt) (vlax-3d-point x)))
                           (cons (trans pt lw0 1) pts))
                     blg (mapcar '(lambda (x) (setq x (if (> (abs x) (* pi)) (- (* 2 pi) (abs x)) x))
                                    (* (if (minusp blg) -1 1) (abs (/ (sin (* 0.25 x)) (cos (* 0.25 x))))))
                           (list (- (car ans) (cadr ans)) (- (last ans) (car ans)))))
               (while (<= i (vlax-curve-getEndParam lw))
		 (if (= (vla-get-ObjectName lw) "AcDbPolyline")
                 (vla-getWidth lw (1- i) 'sw 'ew)
		  (setq sw 0 ew 0)) 
                 (setq bwl (cons (list (vla-getBulge lw (1- i)) sw ew) bwl)
                       i (1+ i)));while
               (setq wl (apply '(lambda (x y) (+ x (* (- par (fix par)) (- y x)))) (cdr (last bwl)))
                     bwl (reverse bwl)
                     bwl (cons (list (car blg) (cadar bwl) wl) bwl)
                     bwl (subst (list (cadr blg) wl (last (cadr bwl))) (cadr bwl) bwl))))
	 );_Pln
	((= (vla-get-ObjectName lw) "AcDb3dPolyline")
	 (setq  crs (vlax-get lw 'Coordinates))
	 (setq bwl (mip-pline-get-segcoors-by-geom lw pt0))
	 (setq par (last bwl))
	 (setq i (cadr bwl) bwl (car bwl))
	 (setq i (trans i 0 1) bwl (trans bwl 0 1) pt0 (trans pt0 0 1))
 	 (setq	pt (trans pt lw0 1))                 ;_27.02.07
	 
	 (setq lst (+ (distance (mapcar '+ bwl '(0 0))(mapcar '+ pt '(0 0)));_Длинна без Z 28.02.07
	              (distance (mapcar '+ pt '(0 0))(mapcar '+ i '(0 0)))
		      )
	       )
	 (setq i (caddr(mapcar '- i bwl))) ;_dZ 28.02.07
	 (setq i (/ i lst)) ;_dZ/L - уклон 28.02.07
	 (setq lst (+ (caddr bwl)(* (distance (mapcar '+ bwl '(0 0))(mapcar '+ pt '(0 0))) i)));_28.02.07
	 (setq	pt (list (car pt)(cadr pt) lst)) ;_22.02.07
	 (setq	pt (trans pt 1 lw0))             ;_22.02.07
	 (setq lst nil bwl nil i 0)
	 (while	(<= i (fix par))
	   (setq lst (reverse
		       (cons (caddr crs)
			     (cons (cadr crs) (cons (car crs) (reverse lst)))))
		 crs (cdddr crs)
		 i   (1+ i)));_while
	 (setq crs (append lst pt crs)) j i);_3dPln
	(t nil)
	);_cond
  (setq	var (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray
		  vlax-vbDouble (cons 0 (1- (length crs)))) crs)))
      (setq i 0)
      (vla-put-Coordinates lw var)
      (if (and (vlax-method-applicable-p lw 'Setbulge) bwl)
	(while (<= j (vlax-curve-getEndParam lw))
                   (vla-setBulge lw (1- j) (caar bwl))
                   (vla-setWidth lw (1- j) (cadar bwl) (caddar bwl))
                   (setq bwl (cdr bwl) j (1+ j))))
  ret)
;|*********************** Команда PL-VxAdd ******************************
*                                                                       *
*   Добавление вершины в полилинию                                      *
*                                                                       *
*************************************************************************
|;
(defun C:PL-VxAdd ( /   break_point selset break_ent
		       break_ent_type *error* pl next osm)
  (setq *error* pltool-err)(setq *PLTOOLS-UNDOLST* nil osm (getvar "OSMODE"))
  (or *pl-activedoc*
      (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
;  (vla-endundomark *pl-activedoc*)
  (vla-startundomark *pl-activedoc*)
  (pl-error-save-sysvar (list '("ANGBASE" 0)'("ANGDIR" 0)'("UCSFOLLOW" 0)'("UCSICON")'("CLAYER")'("osmode" 512)
				 '("CMDECHO" 0)(list "pickbox" (apply 'max  (list 7 (getvar "pickbox"))))
				 (list "aperture" (getvar "pickbox"))))
  (setq	selset (ssget "_I"))
  (vl-cmdf "_convert" "_P" "_all")(setq next t)
  (if selset (SSSETFIRST selset selset))
  (setq selset nil)
  ;; Обработка пустого выбора
  (while next
    (setvar "osmode" 512)
    (initget "Отмени Undo J Г Выход D eXit Ч _U U U U X X X X")
    (setq break_point (getpoint "\nУкажите точку разрыва [отмени U/выход X] <выход> : "))
    (cond
      ((= break_point "U")(if (not (pltools-restorepline))(alert "Нечего отменять")))
      ((= break_point "X")(setq next nil))
      ((null break_point)(setq next nil))
      ((listp break_point)
       (setq selset nil selset (ssget break_point)
            break_ent   (if selset (ssname selset 0) nil)
         break_ent_type (if break_ent (cdr (assoc 0 (entget break_ent))) nil)
	     ) ;_ end of setq
       (if break_ent (setvar "CLAYER" (cdr(assoc 8 (entget break_ent)))))
       (cond ((and break_ent (null (pl:Is-object-modifed break_ent nil)))(alert "Слой блокирован"))
	   ((and break_ent (not(wcmatch (strcase break_ent_type) "*POLYLINE,LINE,ARC,CIRCLE,VIEWPORT")))
	    (alert "Не полилиния, круг, отрезок, дуга!"))
	   ((and break_ent (wcmatch (strcase break_ent_type) "*POLYLINE,LINE,ARC,CIRCLE,VIEWPORT"))
            (setvar "osmode" osm)
;;;       (redraw break_ent 3)
           (setq pl (vlax-ename->vla-object break_ent))
           (if (not (wcmatch (vla-get-ObjectName pl) "*Polyline"))
               (setq break_ent (conv-ent-to-pline break_ent)
		     pl (vlax-ename->vla-object break_ent)))
           (if (wcmatch (vla-get-ObjectName pl) "*Polyline")
	     (progn
	      (pltools-savepline pl)
	      (SSSETFIRST selset selset) 
	    (pl:AddVX (trans break_point 1 0) break_ent
		      (if (setq selset (getpoint break_point "\nУкажите новую точку <прежняя>: "))
			(trans selset 1 0)
			(trans break_point 1 0)))))
;;;           (redraw break_ent 4)
	   )
           ((null break_ent_type)
	      (alert "Ничего не выбрано! Возможно широкая полилиния\nУвельчьте значение PICKBOX или отъедьте подальше зумом")
	    )
           (t nil))       
       )
      (t nil)
      )
    (setq osm (getvar "OSMODE"))
    ) ;_ end of while
  (pl-error-restore-sysvar)(vla-endundomark *pl-activedoc*)
  (SSSETFIRST)(setq *PLTOOLS-UNDOLST* nil)(princ)) ;_ end of defun
;;;Удаление вершин полилиний, которые лежат на одной прямой
;;;http://dwg.ru/forum/viewtopic.php?t=8699

(defun _pl-geom-single-vector (Vekt)
;;; Вычисление орта (единичного вектора)
;;;-------------------------------------
;;;;Параметр: Vekt - любой вектор
;;; Возвращает: Единичный вектор
((lambda (l)
     (if (not(equal 0.0 l 1e-6))
       (mapcar (function (lambda (x) (/ x l))) Vekt)
     )
   )
    (distance '(0 0 0) Vekt)
  )
  )
;;; ************************************************************************
;;; Аргументы:
;;; point — проверяемая точка
;;; arc_start_point - начальная точка дуги,
;;; arc_end_point - конечная точка дуги
;;; arc_center_point - центральная точка дуги
;;; Возвращает t, если точка лежит на дуге, иначе NIL
(defun _pl-arclength (arc_start_point arc_end_point arc_center_point  VNorm / Ang arc_center_point_ocs )
;;; Рассчет длины дуги
;;;-------------------
;;; Аргументы:
;;; point — проверяемая точка
;;; arc_start_point - начальная точка дуги WCS,
;;; arc_end_point - конечная точка дуги WCS
;;; arc_center_point - центральная точка дуги WCS
;;; VNorm - вектор нормали к плоскости дуги (210 поле)
;;; Использует функцию библиотеки
    ;;;                 _dwgru-geom-single-vector
  
;;; Возвращает:
;;;   длину дуги
 (setq VNorm (_pl-geom-single-vector VNorm)
       arc_center_point_ocs  (trans arc_center_point  0 VNorm)
       Ang (- (angle arc_center_point_ocs  (trans arc_end_point 0 VNorm)) (angle arc_center_point_ocs  (trans arc_start_point 0 VNorm)))
 )
 (if (or (< (abs Ang) 1e-6) (equal (abs Ang) (* 2 PI) 1e-6)) (setq Ang 0.0))
 (if (minusp Ang) (setq Ang (+ Ang (* 2 PI))))
 (* (distance arc_center_point  arc_start_point) Ang)
  )

;Got from http://www.theswamp.org/index.php?topic=19865.30
;;;=======================[ PSimple.lsp ]======================= 
;;; Author: Charles Alan Butler (CAB)
;(BULGECENTERRADIUS cblg s_seg e_seg)

(defun BulgeCenterRadius (bulge p1 p2 / delta chord radius center)
    (setq delta  (* (atan bulge) 4)
          chord  (distance p1 p2)
          radius (/ chord (sin (/ delta 2)) 2)
          center (polar p1 (+ (angle p1 p2) (/ (- pi delta) 2)) radius)
    )
  (list (abs radius) center)
  )
(defun tan (a) (/ (sin a) (cos a)))
  (defun pl-RemoveNlst (nlst lst)
    (setq i -1)
    (vl-remove-if  '(lambda (x) (not (null (vl-position (setq i (1+ i)) nlst)))) lst)
  )

(defun pl:VxRdc ( pl tol / vx lst  n ang i SL dH dS pt1 pt2 pt3 pt4 blg vect Remove vxucs etalon aa newb  change_blg )
  (setq pl (pl:conv-ent-to-ename pl))
  ;;;RUS: Исключаем сглаженные (Fit Spline) полилинии
  ;;;Замечен глюк, что если сразу сгладить полилинию, то
  ;;;dxf группа будет показывать на Polyline, а Объектная модель
  ;;;останется LWPOLYLINE с соответствующим вылетом
  ;;;EN We exclude smoothed (Fit Spline) polylines
  (if (not (member (logand (cdr (assoc 70 (entget pl))) (+ 2 4)) '(2 4)))
(progn
   (setq blg (pl-get-coors&width&bulge pl)
       vx (nth 0 blg)
       s_width (nth 1  blg)
       e_width   (nth 2 blg)
       blg (nth 3 blg)
       )
  
(setq vxucs (mapcar '(lambda(x)(trans x pl 1)) vx))
(setq n 1 len (length vx))
(setq pl (pl:conv-ent-to-vla pl))
(if (zerop (car blg))
  (setq etalon (list (setq pt1 (nth 0 vxucs))
                     (setq pt2 (nth 1 vxucs))
                     (angle pt1 pt2)
                     (last (pl:3d_Wnorm pt1 pt2))
                     )
        n 2
        )
  (setq etalon nil)
  )
(while (< n len)
(grtext -1 (strcat "Вершина № "(itoa n)))
  (cond
    ((equal (nth (1- n) vxucs)
            (nth  n vxucs)
            1e-6
            )
     (setq Remove (cons (1- n) Remove))
     )
       ;;;Дуга
     ((and 
        (< n (1- len))
     	(not (equal 0.0 (nth n blg) 1e-6))       ;_Current arc vertex
	(not (equal 0.0 (nth (- n 1) blg) 1e-6)) ;_ Previous 1- arc vertex
           (setq pt3 (BulgeCenterRadius
                       (nth (1- n) blg)
                       (trans (vlax-curve-getPointAtParam pl (1- n)) 0 1)
                       (trans (vlax-curve-getPointAtParam pl n) 0 1)
                       )
                 )
           (setq pt4 (BulgeCenterRadius
                       (nth n blg)
                       (trans (vlax-curve-getPointAtParam pl n) 0 1)
                       (trans (vlax-curve-getPointAtParam pl (1+ n)) 0 1)
                       )
                 )
           (equal (car pt3)(car pt4) 1e-6)   ;_Equal Radius
           (equal (cadr pt3)(cadr pt4) 1e-6) ;_Equal center
        )
            (setq etalon nil)
          ;;  combine the arcs
              (setq i (1- n)) 
              (while (vl-position i Remove)(setq i (1- i))) ;;First not Removed vertex
               (setq pt1 (if (setq aa (assoc i change_blg))
		      (cdr aa)   
		      (nth i blg))
		     )
               (setq aa   (+ (* 4 (atan (abs pt1)))
                             (* 4 (atan (abs (nth n blg)))))
                     newb (tan (/ aa 4.0))
               )
               (if (minusp pt1)
                 (setq newb (- (abs newb)))
                 (setq newb (abs newb))
               )
            (if (setq aa (assoc i change_blg))
	      (setq change_blg (subst (cons i newb) aa change_blg))
	      (setq change_blg (cons (cons i newb) change_blg)))
            (setq         ;_ blg (pl:subst-i i newb blg)
                  Remove (cons n Remove)
                  )
        )
     ((not (equal 0.0 (nth (- n 1) blg) 1e-6)) ;_ Previous 1- arc vertex
      (setq etalon nil)
      )
     ((equal 0.0 (nth (- n 1) blg) 1e-6) ;_Line
      (if (null etalon)
        (setq etalon (list (setq pt1 (nth (1- n) vxucs))
                     (setq pt2 (nth n vxucs))
                     (angle pt1 pt2)
                     (last (pl:3d_Wnorm pt1 pt2))
                     )
              )
        (progn
          (setq pt1 (nth (1- n) vxucs)
                pt2 (nth n vxucs)
                SL (distance pt1 pt2)
                ang (angle pt1 pt2)
                vect (last (pl:3d_Wnorm pt1 pt2))
          )
          (if (apply 'equal (list (minusp vect)(minusp (last etalon)))) ;_Совпадают направления
            (progn
              (setq dS (if (< tol 0)(* (sin (abs tol)) SL) tol))
              (setq
                pt3 (polar pt2 (+ (nth 2 etalon)(* PI 0.5)) 1)
                pt3 (inters pt2 pt3 (nth 0 etalon)(nth 1 etalon) nil))
              (setq dH (if pt3 (distance pt2 pt3) 1.7e99))
              (if (or
                    (equal pt1 pt2 1e-6) ;_Совпадают вершины
                    (equal dH dS 1e-6)
                    (<= dH dS)           ;_Отклонение
                    )
               
                (setq Remove (cons (1- n) Remove))    ;_Удаляем n-1 вершину
                (setq etalon (list (setq pt1 (nth (1- n) vxucs))
                                   (setq pt2 (nth n vxucs))
                                   (angle pt1 pt2)
                                   (last (pl:3d_Wnorm pt1 pt2))
                                   )
                      )
                )
            )
            (setq etalon (list (setq pt1 (nth (1- n) vxucs))
                                   (setq pt2 (nth n vxucs))
                                   (angle pt1 pt2)
                                   (last (pl:3d_Wnorm pt1 pt2))
                                   )
                      )
        );_if
              
        )
        
      )
     )
     (t nil)
    );_cond
  
(setq n (1+ n))
);_while < n len
 ;;;Обработать совпадания точек начала-конца. Если совпадают, то удалить последнюю и включить флаг замкнутости
  (if (and (> (length vxucs) 3)
        (equal (car vxucs)(last vxucs) 1e-6)
       )
    (progn
    (setq Remove (cons (1- (length vxucs)) Remove))
    (vla-put-Closed pl :vlax-true)
    )
    )
  (setq i '-1 SL nil)
  (if change_blg
  (progn  
  (foreach bl blg
    (if (setq aa (cdr(assoc (setq i (1+ i)) change_blg)))
      (setq SL (cons aa SL))
      (setq SL (cons bl SL))
      )
    )
  (setq blg (reverse SL))
    )
    )
  (if Remove
    (progn
      (setq vx (pl-RemoveNlst Remove vx))
      (setq s_width (pl-RemoveNlst Remove s_width))
      (setq e_width (pl-RemoveNlst Remove e_width))
      (setq blg (pl-RemoveNlst Remove blg))
      (PL-SET-COORS&WIDTH&BULGE pl vx s_width e_width blg)
      )
    )
  

  )
    )
(length Remove)
  )
  

   
(defun C:Pl-VxRdc ( / pl *error* ass m util dv tol )
  (setq *error* pltool-err)
  (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark *pl-activedoc*)
  (setq m (ssget "_all") m nil)
  (setq ass (vla-get-ActiveSelectionSet *pl-activedoc*)
	m 0
	util (vla-get-Utility *pl-activedoc*)) 
  (if (< 0 (vla-get-count ass)) (vla-clear ass)) 
  (vla-SelectOnScreen ass (vlax-safearray-fill (vlax-make-safearray vlax-vbinteger '(0 . 0)) '(0)) 
    (vlax-safearray-fill (vlax-make-safearray vlax-vbvariant '(0 . 0)) (list "*Polyline")))
  (initget 2 "H A")
  (setq tol (getdist "\nВведите max отклонение от прямой (H) [угловой допуск A/отклонение H] <0.15>: "))
  (cond ((= tol "H")
	 (initget 3)
	 (setq tol (getdist "\nВведите max отклонение от прямой (H): ")))
	((= tol "A")
	 (initget 3)
	 (setq tol (getangle "\nВведите угловой допуск в градусах (A): ")
	       tol (- 0 tol)))
	((numberp tol) nil)
	(t (setq tol 0.15)))
  
  (if (< 0 (vla-get-count ass)) 
    (vlax-for pl ass
      (setq m (1+ m))
      (if (pl:Is-object-modifed pl nil)
	(progn
	  (setq dv (pl:VxRdc pl tol))
	  (princ  (strcat "\nПолилиния " (itoa m) " обработана: " 
             (itoa dv) " вершин удалено при " (VL-PRINC-TO-STRING (if (< tol 0)(pl:RTD (abs tol)) tol))
               (if (< tol 0)  "-грд. допуске." "-ед. отклонении."))))
	   (princ (strcat "\nПолилиния " (itoa m) " на заблокированном слое"))) 
	  );vlax-for 
    (alert "Ничего не выбрано!"));if
  (vl-cmdf "_redrawall")
  (vla-clear ass)(vlax-release-object ass)
  (vla-endundomark *pl-activedoc*) 
  (princ)) 
;;Удаляет Nvx вершину полилинии pl (Vla object)
;;Нумерация с 0
;;Возвращает t - удалена nil - нет
(defun pl:VxDel	(pl Nvx / vx blglist lst i closed ret)
 (setq closed (vla-get-closed pl)
       Nvx (if (and (eq (vla-get-closed pl) :vlax-true)
		    (= Nvx (vlax-curve-getEndParam pl)))
	     0 Nvx)
       )
 (vla-put-Closed pl :vlax-false)
 (if (eq (vla-get-closed pl) :vlax-true)
     (setq vx (append vx (list (car vx)))))
 (setq vx (lib:pline-get-verts pl)
	blglist	(getblg pl)
	vx	(pl:subst-i Nvx nil vx)
	vx	(vl-remove-if 'null vx)
	lst	(apply 'append vx)) ;setq
  (if (> (length lst) 3.5)
    (progn
  (vla-put-coordinates pl (vlax-make-variant (vlax-safearray-fill
	(vlax-make-safearray vlax-vbDouble (cons 0 (1- (length lst)))) lst)))
;;;Удаляем n вершину заменяем n bulge на nil
  (if blglist (setq blglist (pl:subst-i Nvx nil blglist)))
  (setq blglist (vl-remove-if 'null blglist))
;;;Восстанавливаем bulge
  (setq i 0)
  (if (vlax-method-applicable-p pl 'Setbulge)
    (mapcar '(lambda (blg)(if blg (progn (vla-SetBulge pl i blg) (setq i (1+ i))))) blglist))
  (vla-put-Closed pl closed)
  (setq ret t)
    )
    (progn
      (vla-put-Closed pl :vlax-false)
      (setq ret nil)))
  ret)

(defun c:Pl-VxDel (/ pl *error* pt par next i)
  (setq *error* pltool-err *PLTOOLS-UNDOLST* nil)
  (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object)) next t) 
  (vla-startundomark *pl-activedoc*)
  (while next
    (initget "Отмени Undo J Выход D eXit _U U U X X X")
    (setq pt (entsel "\nУкажите удаляемую вершину полилинии [отмени U/выход X] <выход>: "))
    (cond
      ((null pt)(if (= (getvar "ERRNO") 7)(alert "Ничего не выбрано!!")(setq next nil)))
      ((= pt "X")(setq next nil))
      ((= pt "U")(if (not (pltools-restorepline))(alert "Нечего отменять")))
     ((and (listp pt)
	   (setq pl (vlax-ename->vla-object (car pt)))
	   (setq pt (cadr pt)))
       (if (wcmatch (vla-get-ObjectName pl) "*Polyline") 
      (progn
	(if (pl:Is-object-modifed pl nil)
	  (progn
	(pltools-savepline pl)
	(setq pt (osnap pt "_nea")
	      pt (trans pt 1 0)
	     par (vlax-curve-getParamAtPoint pl pt)
	     par (fix (+ 0.5 par)) ;_Ближайшая вершина
	      )
	(princ "\nВершина № ") (princ (if (and (eq (vla-get-closed pl) :vlax-true)(= par (vlax-curve-getEndParam pl))) 1 (1+ par)))
	(if (pl:VxDel pl par)(princ " удалена")(princ " невозможно удалить"))
	)
	  (princ "\nПолилиния на заблокированном слое")
	  )
        ) 
      (alert "Это не полилиния!!"))
       )
      (t nil)
       
      )
    )
  (vla-endundomark *pl-activedoc*)(SSSETFIRST)
 (setq *PLTOOLS-UNDOLST* nil)(princ))


;;;Возвращает список номеров одинаковых вершин
;;; lst - список вершин полилинии
;;; Проход с конца списка
;;; Удаляются все предыдущие вершины
;;; Кривизна не страдает

(defun pl-geom-uniq-vertex-index ( lst / ret prev this i)
  (setq prev (last lst) i (- (length lst) 2))
  (while (not (minusp i))
    (setq this (nth i lst))
    (if (equal prev this 1e-6)
      (setq ret (cons i ret))
      (setq prev this)
      )
    (setq i (1- i))
    )
  ret
  )

;;;Функция оптимизирует (удаляет одинаковые с точность до 1e-6 знаков вершины полилинии
;;; pl -ename or vla-object
;;; вовращает имя примитива
(defun pl:VxOpt (pl / vx s_width e_width blg remove _func-lstdel)
  (defun _func-lstdel ( lst / ret i)
    (setq i 0)
    (foreach itm lst
      (if (not (vl-position i remove))
        (setq ret (cons itm ret)))
      (setq i (1+ i))
      )
    (reverse ret)
    )
  (and
   (setq blg (pl-get-coors&width&bulge pl)
       vx (nth 0 blg)
       s_width (nth 1  blg)
       e_width   (nth 2 blg)
       blg (nth 3 blg)
       )
  (or (setq Remove (pl-geom-uniq-vertex-index vx)) t)
  (if (and (> (length vx) 3)
        (equal (car vx)(last vx) 1e-6)
       )
    (progn
    (vla-put-Closed (pl:conv-ent-to-vla pl) :vlax-true)
    (setq Remove (cons (1- (length vx)) Remove))
    )
    Remove
    )
    (setq vx (pl-RemoveNlst Remove vx))
    (setq s_width (pl-RemoveNlst Remove s_width))
    (setq e_width (pl-RemoveNlst Remove e_width))
    (setq blg (pl-RemoveNlst Remove blg))
 
  (PL-SET-COORS&WIDTH&BULGE pl vx s_width e_width blg)
   )
  pl
  )
;;;Удаление совпадающих вершин из полилинии
;;;http://dwg.ru/forum/viewtopic.php?t=8722&postdays=0&postorder=asc&start=15
(defun c:PL-VxOpt ( / *error* ss lst)
  (setq *error* pltool-err)
  (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))) 
  (vla-startundomark *pl-activedoc*) 
  (and (setq ss (ssget "_:L" '((0 . "*POLYLINE"))))
       (setq lst (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
       (mapcar 'pl:VxOpt lst)
       )
      (vla-endundomark *pl-activedoc*)
  (princ "\nОбработано ")(princ (length lst))(princ " полилиний")
  (princ)
  )


(defun C:PL-NOARC ( / e1 *error* pl reg ask Rad)
(setq *error* pltool-err flg t)
  (or *pl-activedoc*
       (setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object))))
  (vla-endundomark *pl-activedoc*)
  (vla-startundomark *pl-activedoc*)
  (setq	reg (ssget "_I"))
  (vl-cmdf "_convert" "_P" "_all")
  (if reg (SSSETFIRST reg reg))
    (pl-error-save-sysvar
      (list '("osmode")	'("CLAYER") '("QAFLAGS" 0) '("CMDECHO" 0)))
  (initget 1 "N C L S H")
  (princ "\nВыберите режим апроксимации дуговых сегментов")
  (setq reg nil reg (getkword "\n[количество сегментов N/длина сегмента L/предельное отклонение хорды S/длина хорды C/помощь H]: "))
  (cond ((= reg "N")
	 (initget 7)
	 (setq reg (getint "\nВведите количество сегментов: "))
	 )
	((= reg "L")
	 (initget 7)
	 (setq reg (getdist "\nВведите длину сегмента: ")
	       reg (- 0 reg))
	 )
	((= reg "S")
	 (initget 7)
	 (setq reg (list (getdist "\nВведите предельное отклонение хорды: ")))
	 )
	((= reg "C")
	 (initget 7)
	 (setq reg (getdist "\nВведите длину хорды: ")
	       reg (list(- 0 reg)))
	 )
	((= reg "H")(setq reg nil)
	 (help "pltools.chm" "PLNOARC")
	 )
	(t (setq reg nil)))
(if reg
  (progn
    (initget "Yes No")
    (setq ask (getkword "\nУдалять исходную полилинию [Yes/No] <No>: "))
    (setq ask (= ask "Yes"))
    (initget 6)
    (setq Rad (getdist "\nMin обрабатываемый радиус дуги <любой>: "))
    (if (null Rad)(setq Rad 1e-16))
    )
  )
	  
  (while (and reg (setq e1 (car (entsel "\nВыберите полилинию <хватит>:"))))
    (if (and (setq pl (vlax-ename->vla-object e1))
	      (wcmatch (cdr(assoc 0 (entget e1))) "*POLYLINE")
	      (pl:Is-object-modifed pl nil))
      (mip-pline-no-arc pl reg ask Rad)
      (princ "*** Не 2d, LW полилиния или слой блокирован ***")
      )
    )
(pl-error-restore-sysvar)
(vla-endundomark *pl-activedoc*)
  (princ))
(defun C:PL-CLONE ( / lw pl pl-list par segm AR crs lst blg var *error* sgN Flg)
  (defun *error* (msg)
    (while (> (getvar "CMDACTIVE") 0)(command))  
    (if pl (redraw (vlax-vla-object->ename pl) 4))
    (vl-cmdf "_.redrawall"))
  (defun _clgrdraw ( ptdraw ang color / pt11 pt12 pt21 pt22 len str pt_list tmp sps)
  (setq len (* 0.01 (getvar "VIEWSIZE"))
       pt11 (polar ptdraw (+ ang (pl:DTR 225)) len)
       pt12 (polar ptdraw (+ ang (pl:DTR 45)) len)
       pt21 (polar ptdraw (+ ang (pl:DTR 315)) len)
       pt22 (polar ptdraw (+ ang (pl:DTR 135)) len))
  (grvecs (list color pt11 pt12 pt21 pt22))
  (if (and (> (pl:RTD ang) 90.0) (<= (pl:RTD ang) 270.0))(setq ang (+ ang PI))) ;_ end of if  
  (setq ptdraw (polar ptdraw ang len))
  (setq str (itoa sgN))
  (setq str (VL-STRING->LIST str) sps (* 0.7 len))
  (foreach nb str
    (cond ((= nb 48) ;_0
	   (setq pt_list (append pt_list (list
              (list
		ptdraw
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) len))
		tmp
		(setq tmp (polar tmp ang (* 0.5 len)))
		tmp
		(setq tmp (polar tmp (+ ang (* 1.5 PI)) len))
		tmp ptdraw))))
	   (setq ptdraw (polar ptdraw ang sps))
	   )
	  ((= nb 49) ;_1
	   (setq pt_list (append pt_list (list
              (list
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI))(* 0.5 len)))
		(setq tmp (polar (setq tmp (polar tmp ang (* 0.5 len)))(+ ang (* 0.5 PI))(* 0.5 len)))
		tmp
		(setq tmp (polar tmp (+ ang (* 1.5 PI)) len))
		))))
	   (setq ptdraw (polar ptdraw ang sps))
	   )
	  ((= nb 50) ;_2
	   (setq pt_list (append pt_list (list
              (list
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) len))
		(setq tmp (polar tmp ang (* 0.5 len)))
		tmp
		(setq tmp (polar tmp (+ ang (* 1.5 PI))(* 0.5 len)))
		tmp ptdraw ptdraw
		(setq tmp (polar ptdraw ang (* 0.5 len)))
		))))
	   (setq ptdraw (polar ptdraw ang sps))
	   )
	  ((= nb 51) ;_3
	   (setq pt_list (append pt_list (list
              (list
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) len))
		(setq tmp (polar tmp ang (* 0.5 len)))
		tmp
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI))(* 0.5 len)))
		tmp
		(setq tmp (polar tmp ang (* 0.5 len)))
		 tmp ptdraw 
		))))
	   (setq ptdraw (polar ptdraw ang sps))
	   )
	  ((= nb 52) ;_4
	   (setq pt_list (append pt_list (list
              (list
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) len))
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) (* 0.5 len)))
		tmp
		(setq tmp (polar tmp ang (* 0.5 len)))
		(setq tmp (polar tmp (+ ang (* 0.5 PI))(* 0.5 len)))
		(setq tmp (polar ptdraw ang (* 0.5 len)))
		))))
	   (setq ptdraw (polar ptdraw ang sps))
	   )
	  ((= nb 53) ;_5
	   (setq pt_list (append pt_list (list
              (list
		(setq tmp (polar (setq tmp (polar ptdraw ang (* 0.5 len)))(+ ang (* 0.5 PI)) len))
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) len))
		tmp
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) (* 0.5 len)))
		tmp
		(setq tmp (polar tmp ang (* 0.5 len)))
		tmp
		(setq tmp (polar ptdraw ang (* 0.5 len)))
		tmp ptdraw
		))))
	   (setq ptdraw (polar ptdraw ang sps))
	   )
	  ((= nb 54) ;_6
	   (setq pt_list (append pt_list (list
              (list
		(setq tmp (polar (setq tmp (polar ptdraw ang (* 0.5 len)))(+ ang (* 0.5 PI)) len))
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) (* 0.5 len)))
		tmp
		(setq tmp (polar tmp ang (* 0.5 len)))
		tmp
		(setq tmp (polar ptdraw ang (* 0.5 len)))
		tmp ptdraw ptdraw
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) (* 0.5 len)))
		))))
	   (setq ptdraw (polar ptdraw ang sps))
	   )
	  ((= nb 55) ;_7
	   (setq pt_list (append pt_list (list
              (list
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) len))
		(setq tmp (polar tmp ang (* 0.5 len)))
		tmp
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) (* 0.5 len)))
		tmp ptdraw
		))))
	   (setq ptdraw (polar ptdraw ang sps))
	   )
	  ((= nb 56) ;_8
	   (setq pt_list (append pt_list (list
              (list
		ptdraw
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) len))
		tmp
                (setq tmp (polar tmp ang (* 0.5 len)))
		tmp
		(setq tmp (polar ptdraw ang (* 0.5 len)))
		tmp
		ptdraw
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) (* 0.5 len)))
		(setq tmp (polar tmp ang (* 0.5 len)))
		))))
	   
	   (setq ptdraw (polar ptdraw ang sps))
	   )
	  ((= nb 57) ;_9
	   (setq pt_list (append pt_list (list
              (list
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) (* 0.5 len)))
		(setq tmp (polar ptdraw (+ ang (* 0.5 PI)) len))
		tmp
		(setq tmp (polar tmp ang (* 0.5 len)))
		tmp
		(setq tmp (polar tmp (+ ang (* 1.5 PI)) (* 0.5 len)))
		tmp
                (polar ptdraw (+ ang (* 0.5 PI)) (* 0.5 len))
		tmp ptdraw
		))))
               (setq ptdraw (polar ptdraw ang sps))
	   )
	  (t nil)
	  )
    )
    (foreach pt pt_list   (grvecs (append (list color) pt)))
  )
(initget (if AR "A" "R"))(setq pl-list nil Flg t)
(while Flg
   (setq lw (entsel
		  (strcat "\n Выберите сегмент полилинии для "
		  (if AR "удаления из списка" "добавления в список")
		  " ["
		   (if AR "добавить A]" "удалить R]") " <готово>:")))
  (cond ((null lw)(if (= (getvar "ERRNO") 52)(setq Flg nil)(princ " *Мимо*")))
       ((member lw '("A" "R"))(setq AR (not AR)))
        ((wcmatch (cdr (assoc 0 (entget (car lw)))) "*POLYLINE")
	 (if (pl:Is-object-modifed (car lw) nil)
	 (progn  
	 (if (or (null pl-list)(eq (car lw)(car pl-list)))
	   (progn
	     (if (null pl-list)
	       (progn (redraw (car lw) 3)
		 (setq pl-list (list (car lw)))))
	     (setq pl (vlax-ename->vla-object (car lw))
		  par (vlax-curve-getParamAtPoint pl
			(vlax-curve-getClosestPointTo pl (trans (cadr lw) 1 0)))
		  par (fix par) 
		 segm (list
			(vlax-curve-getPointAtParam pl (fix par))
			(vlax-curve-getPointAtParam pl (1+(fix par)))
			(angle (trans (vlax-curve-getPointAtParam pl (fix par)) 0 1)
			       (trans (vlax-curve-getPointAtParam pl (1+ (fix par))) 0 1))
			(+ par 0.5)
			(if (vlax-method-applicable-p pl 'getbulge)
			  (VL-CATCH-ALL-APPLY 'vla-GetBulge (list pl par)) nil))
	      pl-list (if AR (progn (if (member segm pl-list)
				      (princ "\n *удален из списка*")
				      (princ "\n *нет в списке*"))
				      (setq pl-list (vl-remove segm pl-list)))
			(if (member segm pl-list)(progn (princ "\n *уже в списке*") pl-list)
			    (append pl-list (list segm)))))
	     (vl-cmdf "_REDRAW")(setq sgN 0)
	     (foreach item (cdr pl-list)(setq sgN (1+ sgN))
	       (_clgrdraw (trans (vlax-curve-getPointAtParam pl (nth 3 item)) 0 1)(nth 2 item) 1))
	     )
	     (princ "\n *не та полилиния*")
	   )
	   (princ " всего ")(princ(1- (length pl-list)))(princ " сегментов")
	   )
	   (princ "\n *слой блокирован*"))
	 )
	(t (princ "\n *не полилиния*"))
	)
  (initget (if AR "A" "R"))
  )
  (if (cdr pl-list)
    (progn
      (redraw (car pl-list) 4)
      (setq pl (vlax-ename->vla-object (car pl-list))
	    pl-list (cdr pl-list)
	    crs (list (caar pl-list))
	    blg (list (last (car pl-list)))
	    segm (car pl-list)
	    pl-list (cdr pl-list)
	    crs (append crs (list(cadr segm)))
	    )
      (foreach sg pl-list
	(setq blg (append blg (list (last sg))))
	(setq var (mapcar '- (cadr sg)(car sg)))
	(setq var (mapcar '+ (last crs) var))
	
	(setq crs (append crs (list var)))
	)
  ;;;    (setq crs (mapcar '(lambda(x)(trans x 1 0)) crs))
      (if (= (vla-get-ObjectName pl) "AcDb3dPolyline")
	(setq lst crs)
	(setq crs (mapcar '(lambda(x)(trans x 0 (vlax-vla-object->ename pl))) crs)
	      lst (mapcar '(lambda (x)(list (car x)(cadr x))) crs)))
        (setq crs (apply 'append lst))
        (setq	var (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray
		  vlax-vbDouble (cons 0 (1- (length crs)))) crs))
		lst (vla-ObjectIDToObject (vla-get-activedocument (vlax-get-acad-object))(vla-get-OwnerID pl))
		)
      (if (= (vla-get-ObjectName pl) "AcDb3dPolyline")
	(setq lw (vla-Add3DPoly lst var))
	(progn
	  (setq lw (vla-addLightWeightPolyline lst var))
	  (vla-put-elevation lw (vla-get-elevation pl)) 
	  
	  )
	)
      (mapcar '(lambda (x y)(vlax-put-property lw x y))
		'(Linetype LineWeight Color Layer)
		(mapcar '(lambda (x)
			 (vlax-get-property pl x))
			'(Linetype LineWeight Color Layer)))
      (setq segm '-1)
      (if (and (vlax-method-applicable-p lw 'Setbulge) blg)
      (foreach bl blg (if (numberp bl)(vla-setBulge lw (setq segm (1+ segm)) bl))))
      )
    )
  (vl-cmdf "_.redrawall")(princ))
(defun C:PL-VxMove ( / *error* next ent-pline pl Npt Ovx par vx lst undolst what str pdr nab pt lockZ
		       ptWCS ptENT cZ)
(setq *error* pltool-err)
(setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object)) next t)
(setvar "CMDECHO" 0)
(vla-startundomark *pl-activedoc*)
(while next
  (SSSETFIRST)
  (vl-cmdf "_redrawall")
  (if (setq ent-pline (entsel "\nУкажите изменяемую вершину полилинии <выход>: "))
  (if (and (pl:Is-object-modifed (car ent-pline) nil)
        (member (vla-get-ObjectName (setq pl (vlax-ename->vla-object (car ent-pline))))
                '("AcDb2dPolyline" "AcDbPolyline" "AcDb3dPolyline")))
    (progn
      (setq nab nil nab (ssadd))
      (ssadd (car ent-pline) nab)
      (SSSETFIRST nab nab)(setq nab nil)
      (setq Npt (osnap (cadr ent-pline) "_nea"))
      (setq  Npt (trans Npt 1 0)
	     par (vlax-curve-getParamAtPoint pl Npt)
	     pdr (+ 0.5 (fix par))
	     par (fix (+ 0.5 par)) ;_Ближайшая вершина
    	     par (if (and (eq (vla-get-closed pl) :vlax-true)
		    (= par (vlax-curve-getEndParam pl))) 0 par)
             Ovx (vlax-curve-getPointAtParam pl par)
	      Ovx (trans Ovx 0 1)
              cZ (caddr Ovx)
	      what 1+
	      str "следующая"
	      undolst nil
	      )
(mip_grdraw (setq Npt (trans (vlax-curve-getPointAtParam pl pdr) 0 1))
	 (angle Npt (trans (vlax-curve-getPointAtParam pl (+ pdr 0.0001)) 0 1)) 1)
      (if (=(vla-get-ObjectName pl) "AcDb3dPolyline")
       (progn
	 (initget "Да Нет Yes No L Y _Yes No Yes No Yes No")
	 (setq lockZ (member
		       (getkword "\nСохранять Z узла полилинии [да Y/ нет N] <Да>: ")
		       '("Yes" nil)))
	 )
       )
      (_vxgrdraw Ovx -1)
      (initget "N Т Следующий Next P З Предыдущий Prev U Г Отмени Undo X Ч Выход Exit _N N N N P P P P U U U U X X X X")
      (while (/= "X" (setq Npt (getpoint Ovx
				(strcat "\nНовое положение вершины " (itoa (1+ (fix par)))
				        " [следующий N/предыдущий P/отмени U/выход X] <" str ">:"))))
	(cond
	  ((null Npt) nil)
	  ((= Npt "N")(setq what 1+ str "следующая"))
  ;;	  ((= Npt "B")(pl-vx1 pl (fix par))(setq par 0))
	  ((= Npt "P")(setq what 1- str "предыдущая"))
	  ((listp Npt)
	   (setq ptWCS (trans Npt 1 0)
		 ptENT (trans Npt 1 (car ent-pline))
	         par (if (and (eq (vla-get-closed pl) :vlax-true)
	               (= par (vlax-curve-getEndParam pl))) 0 par)
             Ovx (vlax-curve-getPointAtParam pl par)
            Ovx (trans Ovx 0 1)
             cZ (caddr Ovx)
	      vx (lib:pline-get-verts pl)
	 undolst (append (list(list par (apply 'append vx))) undolst)
	      vx (pl:subst-i par
			  (cond
			    ((= (vla-get-ObjectName pl) "AcDbPolyline")
			     (list (car ptENT)(cadr ptENT)))
			    ((=(vla-get-ObjectName pl) "AcDb3dPolyline")
			     (if lockZ
                               (progn
                                 ;(setq Npt (trans Npt 1 0))
				 (trans (list (car Npt)(cadr Npt) cZ) 1 0)
                                 )
			       ptWCS)
			     )
			    ((=(vla-get-ObjectName pl) "AcDb2dPolyline")  ptENT)
			    (t (trans Ovx 1 0)))
			   vx)
	      lst (apply 'append vx))
	   (vla-put-coordinates pl (vlax-make-variant (vlax-safearray-fill
             (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length lst)))) lst)))
	   (vl-cmdf "_redrawall")
	   (mip_grdraw (setq Npt (trans (vlax-curve-getPointAtParam pl pdr) 0 1))
	 (angle Npt (trans (vlax-curve-getPointAtParam pl (+ pdr 0.0001)) 0 1)) 1)

	  )
	   ((= Npt "U")
	    (setq lst (car undolst)
		  undolst (cdr undolst))
	    (if lst
	      (progn
		(setq par (car lst)
		      lst (cadr lst))
	      (vla-put-coordinates pl (vlax-make-variant (vlax-safearray-fill
                (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length lst)))) lst)))
		(setq Ovx (trans (vlax-curve-getPointAtParam pl par) 0 1))
		(vl-cmdf "_redrawall")
	      (mip_grdraw (setq pt (trans (vlax-curve-getPointAtParam pl pdr) 0 1))
	 (angle pt (trans (vlax-curve-getPointAtParam pl (+ pdr 0.0001)) 0 1)) 1)

	      )
	      (alert "Нечего отменять")
	      )
	    )
	  (t nil)
	  )
	(_vxgrdraw Ovx 0)
        (if (not (member Npt '("U" "B")))(setq par (what par)))
        (if (> par (vlax-curve-getEndParam pl))(setq par 0))
        (if (< par 0)(setq par (if (eq (vla-get-closed pl) :vlax-true)
				 (1- (vlax-curve-getEndParam pl))
				 (vlax-curve-getEndParam pl))))
	(setq par (if (and (eq (vla-get-closed pl) :vlax-true)
		    (= par (vlax-curve-getEndParam pl))) 0 par))
	(setq Ovx (trans (vlax-curve-getPointAtParam pl par) 0 1))
	(_vxgrdraw Ovx -1)
	(initget "N Т Следующий Next P З Предыдущий Prev U Г Отмени Undo X Ч Выход Exit _N N N N P P P P U U U U X X X X")
	)
      )
    (princ "*** Объект на блокированном слое или не полилиния ***")
    )
    (setq next nil))
);_while
(vl-cmdf "_redrawall")(SSSETFIRST)
(vla-endundomark *pl-activedoc*)  
(princ))

;;pl - vla полилиния
;;par - начальная вершина, начиная с 0
(defun pl-vx1 (pl par / vx vx1 blg blg1 vxt blgt lst i start_width end_width start_width1 end_width1 start_widthT end_widthT)
  
 (setq blg (pl-get-coors&width&bulge pl)
       vx (nth 0 blg)
       start_width (nth 1  blg)
       end_width   (nth 2 blg)
       blg (nth 3 blg)
       )
  
  (setq i 0 vx1 vx blg1 blg start_width1 start_width end_width1 end_width vxt nil blgt nil)
  (while (< i par)
    (setq vxt (append vxt (list (car vx1)))
	  vx1 (cdr vx1)
	  blgt (append blgt (list (car blg1)))
	  blg1 (cdr blg1)
	  start_widthT (append start_widthT (list (car start_width1)))
	  start_width1 (cdr start_width1)
	  end_widthT (append end_widthT (list (car end_width1)))
	  end_width1 (cdr end_width1)
	  i (1+ i)
	  )
    )
  (setq vx (append vx1 vxt)
	blg (append blg1 blgt)
	start_width (append start_width1 start_widthT)
	end_width (append end_width1 end_widthT))
  (setq lst (apply 'append vx))
  (PL-SET-COORS&WIDTH&BULGE pl vx start_width end_width blg)
  )
(defun C:PL-Vx1 ( / *error* next ent-pline pl par doit Npt Ovx pdr nab)
(setq *error* pltool-err)
(setq *pl-activedoc* (vla-get-activedocument (vlax-get-acad-object)) next t)
(setvar "CMDECHO" 0)
(vla-startundomark *pl-activedoc*)
(while next
  
  (vl-cmdf "_redrawall")
  (initget "Отмени Undo J Г Выход D eXit У _U U U U X X X X")
  (setq ent-pline (entsel "\nУкажите новую начальную вершину полилинии [отмени U/выход X] <выход>: "))
  (cond
     ((null ent-pline)(if (= (getvar "ERRNO") 7)(alert "Ничего не выбрано!!")(setq next nil)))
     ((= ent-pline "X")(setq next nil))
     ((= ent-pline "U")(if (not (pltools-restorepline))(alert "Нечего отменять")))
     ((and ent-pline (pl:Is-object-modifed (car ent-pline) nil)
        (member (vla-get-ObjectName (setq pl (vlax-ename->vla-object (car ent-pline))))
                '("AcDb2dPolyline" "AcDbPolyline" "AcDb3dPolyline")))
      (setq nab nil nab (ssadd))
      (ssadd (car ent-pline) nab)
      (SSSETFIRST nab nab)(setq nab nil)
      (setq Npt (osnap (cadr ent-pline) "_nea"))
      (setq  Npt (trans Npt 1 0)
	     Npt (vlax-curve-getclosestpointto pl Npt)
	     par (vlax-curve-getParamAtPoint pl Npt)
	     pdr (+ 0.5 (fix par))
	     par (fix (+ 0.5 par)) ;_Ближайшая вершина
    	     par (if (and (eq (vla-get-closed pl) :vlax-true)
		    (= par (vlax-curve-getEndParam pl))) 0 par)
	      Ovx (trans (vlax-curve-getPointAtParam pl par) 0 1)
	      )
(mip_grdraw (setq Npt (trans (vlax-curve-getPointAtParam pl pdr) 0 1))
      (angle Npt (trans (vlax-curve-getPointAtParam pl (+ pdr 0.0001)) 0 1)) 1)
      (_vxgrdraw Ovx -1)
	 (initget "Да Нет Yes No L Y _Yes No Yes No Yes No")
	 (setq doit (member
		       (getkword (strcat "\nСделать началом полилинии вершину №" (itoa (1+ par)) " ? [да Y/нет N] <Да>: "))
		       '("Yes" nil )))
      (if doit
	(progn
	(pltools-savepline pl)
	(pl-vx1 pl par)
	)
	)
      )
     (t (princ "*** Объект на блокированном слое или не полилиния ***"))
    );_cond
);_while
(vl-cmdf "_redrawall")(SSSETFIRST)
(vla-endundomark *pl-activedoc*)  
(princ))

;;http://www.theswamp.org/index.php?topic=19241.0
(defun C:R3P ( / *error* pt1 pt2 pt1W pt2W ucs ucf isRus)
  (defun *error* (msg)(princ msg)
    (if ucs
    (progn
      (command "_.UCS" "_R" "TmpUcs")
      (command "_.UCS" "_D" "TmpUcs")
      )
    )
    (setvar "UCSFOLLOW" ucf)(vla-endundomark *adoc*)
    (princ)) ;_ end of defun
  (vl-load-com)
(or *adoc* (setq *adoc* (vla-get-activedocument (vlax-get-acad-object))))
(setvar "CMDECHO" 0)(setvar "EXPERT" 5)
(vla-startundomark *adoc*)
 (command "_.UCS" "_Save" "TmpUcs")
  (setq isRus (= (getvar "DWGCODEPAGE") "ANSI_1251")
          ucf (getvar "UCSFOLLOW"))
  (setvar "UCSFOLLOW" 0)
  (initget 1)
  (setq pt1 (getpoint
              (if isRus
                "\nПервая точка прямоугольника:"
                "\nThe first point of a rectangular:"
              ) ;_ end of if
            ) ;_ end of getpoint
  ) ;_ end of setq
  (setq pt1W (trans pt1 1 0))
  (initget 1 "Angle Угол _Angle Angle")
  (setq pt2 (getpoint pt1
                      (if isRus
                        "\nВторая точка прямоугольника [Угол]:"
                        "\nThe second point of a rectangular [Angle]:"
                      ) ;_ end of if
            ) ;_ end of getpoint
  ) ;_ end of setq
  (if (= pt2 "Angle")
    (progn
      (if (null (setq pt2 (getangle
                            (if isRus
                              "\nУкажите угол <0>: "
                              "\nEnter new angle <0>: "
                              ) ;_ end of if
                            )
                      )
                )
        (setq pt2 0)
        )
      (setq ucs (vl-cmdf "_.UCS"
                     "_N"
                     "_3"
                     "_non" pt1
                     "_non" (setq pt2 (polar pt1 pt2 10))
                     "_non" (polar pt2 (+ (angle pt1 pt2) (* 0.5 PI)) 10)
            ) ;_ end of vl-cmdf
  ) ;_ end of setq
        (setvar "orthomode" 1)
        (initget 1)
        (setq pt2 (getpoint (setq pt1 (trans pt1W 0 1))
                      (if isRus
                        "\nВторая точка прямоугольника:"
                        "\nThe second point of a rectangular:"
                      ) ;_ end of if
            ) ;_ end of getpoint
  ) ;_ end of setq
      
      );_progn
      )
  (setq pt1W (trans pt1 1 0)
        pt2W (trans pt2 1 0))
  (setq ucs (vl-cmdf "_.UCS"
                     "_N"
                     "_3"
                     "_non" pt1
                     "_non" pt2
                     "_non" (polar pt2 (+ (angle pt1 pt2) (* 0.5 PI)) 10)
            ) ;_ end of vl-cmdf
  ) ;_ end of setq
    (setvar "CMDECHO" 1)
  (command "_.RECTANGLE"
           "_non" (trans pt1W 0 1)
           ".X"
           "_non"  (trans pt2W 0 1)
  ) ;_ end of command
  (while (> (getvar "CMDACTIVE") 0)(command pause))
  (if ucs
    (progn
      (command "_.UCS" "_R" "TmpUcs")
      (command "_.UCS" "_D" "TmpUcs")
      )
    ) ;_ end of if
  (setvar "UCSFOLLOW" ucf)
(vla-endundomark *adoc*)
  (princ)
) ;_ end of defun
 
(defun C:PL-SgInfo ( / dis ent fpar pickpt spar pline curpt blglist blg rad cen p1 p2 str isRus *error*) 
(vl-load-com)(setq *error* pltool-err)
(setq isRus (= (getvar "DWGCODEPAGE") "ANSI_1251"))
(while (and (setq ent (entsel (if isRus
     "\nУкажите точку на интересующем сегменте (Enter - выход): "
     "\nPick segment point to be dimensioned (or press Enter to exit loop) : ")))
            (wcmatch (cdr(assoc 0 (entget(car ent)))) "*POLYLINE")
            )
(setq vobj (vlax-ename->vla-object (car ent))) 
(setq pickpt (trans (cadr ent) 1 0)) 
(setq curpt (vlax-curve-getclosestpointto vobj pickpt)) 
(setq fpar (1+ (fix (vlax-curve-getparamatpoint vobj curpt))) 
      spar (1- fpar)) 
(setq dis (distance 
   (setq p2 (vlax-curve-getpointatparam vobj fpar)) 
   (setq p1 (vlax-curve-getpointatparam vobj spar)))) 
(setq blglist (getblg (car ent)) blg (nth spar blglist)) 
(setq rad (lib:pline-get-radii p1 p2 blg)) 
(setq cen (lib:pline-get-segm-center vobj p1 p2 blg))
(if isRus
(princ "\n\n**** Информация о сегменте: ****\n")   
(princ "\n\n**** Segment info: ****\n"))
(if isRus  
(setq str (strcat "№ сегмента : " (VL-PRINC-TO-STRING fpar) 
"\nДлина сегмента: " (rtos dis 2 5)
"\nРадиус: "  (rtos rad 2 5) 
"\nЦентр сегмента в МСК: "(VL-PRINC-TO-STRING cen) 
"\nНачало сегмента в МСК: "(VL-PRINC-TO-STRING p1) 
"\nКонец сегмента в МСК: "(VL-PRINC-TO-STRING p2)
"\nЦентр сегмента в ПСК: "(VL-PRINC-TO-STRING (trans cen 0 1)) 
"\nНачало сегмента в ПСК: "(VL-PRINC-TO-STRING (trans p1 0 1)) 
"\nКонец сегмента в ПСК: "(VL-PRINC-TO-STRING (trans p2 0 1))
                  )
      )
(setq str (strcat "Segment number : " (VL-PRINC-TO-STRING fpar) 
"\nSegment length is: " (rtos dis 2 5) " metric dwg units" 
"\nRadius: "  (rtos rad 2 5) 
"\nSegment center point in WCS: "(VL-PRINC-TO-STRING cen) 
"\nSegment start point in WCS: "(VL-PRINC-TO-STRING p1) 
"\nSegment end point in WCS: "(VL-PRINC-TO-STRING p2)
"\nSegment center point in UCS: "(VL-PRINC-TO-STRING (trans cen 0 1)) 
"\nSegment start point in UCS: "(VL-PRINC-TO-STRING (trans p1 0 1)) 
"\nSegment end point in UCS: "(VL-PRINC-TO-STRING (trans p2 0 1))
                  ))
  )
(princ str)(pl:mydcl (if isRus "Информация о сегменте" (princ "Segment info"))
             (pl:str-str-lst str "\n")))(princ))

;Middle PolyLine (MPL)
;http://dwg.ru/forum/viewtopic.php?t=148&postdays=0&postorder=asc&start=15
(defun C:MPL (/ adoc *error* crvs eps dL pts pt1 pt2 ptc n osm param intrs)
  (defun *error* (msg)(princ msg)
    (vla-Regen adoc acActiveViewport)
    (vla-EndUndoMark adoc)
    (setvar "OSMODE" osm)
  )
  (vl-load-com)
  (setq osm (getvar "OSMODE"))
  (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark adoc)
  (setvar "CMDECHO" 0)
  (setq	crvs (mapcar '(lambda (y / en)
			(setq en
			       (car (entsel (strcat "\nВыберите " y " границу: ")))
			)
			(if en
			  (redraw en 3)
			)
			en
		      )
		     '("первую" "вторую")
	     )
  )
    
  (if (vl-some 'null crvs)
    (progn
 (mapcar '(lambda (x) (if x (redraw x 4))) crvs)        
    (alert "Недобор!!!")
    )
    (progn
      (mapcar '(lambda (x) (redraw x 4)) crvs)
      (setq crvs (mapcar 'vlax-ename->vla-object crvs))
      (if (apply
	    'and
	    (mapcar
	      '(lambda (x)
		 (wcmatch (vla-get-ObjectName x) "*Polyline,*Spline,*Line")
	       )
	      crvs
	    )
	  )
	(progn

	  (setq intrs (apply 'pl:GetInters   (append  crvs (list acExtendNone))))
	  (setq	PARAM (mapcar '(lambda ( crv / ret)
				 (foreach pt intrs
				 (setq ret (append ret (list (vlax-curve-getParamAtPoint crv pt)))))
				 ret)
				    crvs 
			      )
		)
          (setq PARAM (mapcar '(lambda ( par crv)
                                   (append (list (vlax-curve-getStartParam crv))
                                           par
                                           (list (vlax-curve-getEndParam crv))
                                           )
                                   
                                    )
                              PARAM crvs
                              )
                )
          
          (setq PARAM (mapcar 'pl:mkseg PARAM))
	  (initget 6)
	  (setq	dL
		 (if
		   (setq dL (getint "\nКоличество опорных точек <100>: "))
		    dL
		    100
		 )
	  )
          
;(setq ParLst (car param) crv (car crvs))
(setq pts (mapcar '(lambda ( ParLst crv / sps dp)          
(foreach par Parlst
    (setq dp (- (last par)(car par))
          dp (/ dp dl))
    (setq n 0)
    (while (< n dl)
        (setq pr (+ (car par)(* n dp)))
        (setq sps (append sps (list (vlax-curve-getPointAtParam crv pr))))
        (setq n (1+ n))
        )
    )
 (setq sps (append sps (list (vlax-curve-getEndPoint crv))))
)
        PARAM crvs)
      )
	  
(setq	pt1 (car pts) pt2 (cadr pts)
        pt1 (mapcar '(lambda(x)(trans x 0 1)) pt1)
        pt2 (mapcar '(lambda(x)(trans x 0 1)) pt2))
	  (if (> (+ (distance (car pt1) (car pt2))
		    (distance (last pt1) (last pt2))
		 )
		 (+ (distance (car pt1) (last pt2))
		    (distance (last pt1) (car pt2))
		 )
	      )
	    (setq pt2 (reverse pt2))
	  )
	  (setq
	    ptc	(mapcar
		  '(lambda (t1 t2)
		     (polar t1 (angle t1 t2) (* 0.5 (distance t1 t2)))
		   )
		  pt1
		  pt2
		)
	  )
	  (setvar "OSMODE" 0)
	  ;(setq ptc (mapcar '(lambda (x)(trans x 0 1)) ptc))
	  (vl-cmdf "_.PLINE" (car ptc) "_W" 0 0)
	  (foreach pt (cdr ptc) (vl-cmdf pt))
	  (vl-cmdf "")
	)
      )
    )
     
  ) ;_if apply
  (setvar "OSMODE" osm)
  (vla-EndUndoMark adoc)
  (princ)
)

;|
******************************************************************************
*                                                                            *
*   Special thanks for ideas and codes of programs                           *
*   Joe Burke TraceObject  (http://www.theswamp.org/index.php?topic=13842.0) *
*                                                                            *
*   Особая благодарность за идеи и коды программ:                            *
*   Joe Burke TraceObject  http://www.theswamp.org/index.php?topic=13842.0   *
*                                                                            *
******************************************************************************
|;


;; Joe Burke - 11/24/2006
;; Edit 15.02.2006 V. Azarko
;; Combined former TracePline, TraceACE and TraceSpline functions.
;;http://www.theswamp.org/index.php?topic=13842.0

;; Added TraceLine, TraceType1Pline, TraceType23Pline
;; and Trace3DPline.

;; The deg argument used in the old TracePline and TraceACE 
;; functions is now hard-coded into those fuctions using a
;; value of 7.5 degrees.

;; Lightweight and heavy plines are tested first to see if they
;; contain arcs. If not, the point list returned is derived from
;; the Coordinates property.

;; Function not used.
;(defun CheckForBulge (obj / param endparam flag)
;  (setq param (vlax-curve-getStartParam obj)
;        endparam (vlax-curve-getEndParam obj)
;  ) 
;  ;; not interested in last bulge - right?
;  (while (< param endparam)
;    (if (/= 0 (vla-getbulge obj param))
;      (setq flag T)
;    )
;    (setq param (1+ param))
;  )
;  flag
;) ;end

;; Toolbox functions included: NormalAngle and @delta. 
;; External functions not included: none.

;; Argument: ename or vla-object of a line, lwpline, heavy pline,
;;           3D pline, arc, circle, ellipse or spline. Curve fit, 
;;           quadratic and cubic plines are allowed.
;; Returns:  3D WCS point list. The Z value of all points is 0.0.
;;           The sub-functions return point list, not the primary function.
;; Notes:    When a 3D point list is passed to entmake lwpline the
;;           Z value is ignored.

(defun TraceObject (obj / typlst typ TracePline TraceACE TraceLine 
                         TraceSpline TraceType1Pline 
                          TraceType23Pline)

  ;;;; start sub-functions ;;;;

    (defun ZClosed (lst)
    (if (and (vlax-curve-isClosed obj)
	     (not(equal (car lst)(last lst) 1e-6)))
      (append lst (list (car lst)))
      lst
      )
    )
  ;; Argument: vla-object, a heavy or lightweight pline.
  ;; Returns: WCS point list if successful.
  ;; Notes: Duplicate adjacent points are removed.
  ;;        The last closing point is included given a closed pline.
  (defun TracePline (obj / param endparam anginc tparam pt blg 
                           ptlst delta inc arcparam flag)

    (setq param (vlax-curve-getStartParam obj)
          endparam (vlax-curve-getEndParam obj)
          anginc (* pi (/ 7.5 180.0))
    )

    ;; Check to see if the pline contains arcs.
    ;; If not, get the point list from the coordinates property.
    ;; If so, do the other stuff.
    ;; Don't modify the param var here.
    (setq tparam param)
;;;    (while (< tparam endparam)
;;;      (if (/= 0 (vla-getbulge obj tparam))
;;;        (setq flag T)
;;;      )
;;;      (setq tparam (1+ tparam))
;;;    )
    
;;;    (if (not flag)
;;;      (progn
;;;        (setq coord (vlax-get obj 'Coordinates))
;;;        (if (eq "AcDbPolyline" (vlax-get obj 'ObjectName))
;;;          (repeat (/ (length coord) 2)
;;;            (setq ptlst (cons (list (car coord) (cadr coord) 0.0) ptlst)
;;;                  coord (cddr coord)
;;;            )
;;;          )
;;;          (repeat (/ (length coord) 3)
;;;            (setq ptlst (cons (list (car coord) (cadr coord)(caddr coord)) ptlst)
;;;                  coord (cdddr coord)
;;;            )
;;;          )
;;;        )
;;;      )
      ;else
      (while (<= param endparam)
        (setq pt (vlax-curve-getPointAtParam obj param))
        ;Avoid duplicate points between start and end.
        (if (not (equal pt (car ptlst) 1e-12))
          (setq ptlst (cons pt ptlst))
        )
        ;A closed pline returns an error (invalid index) 
        ;when asking for the bulge of the end param.
        (if 
          (and 
            (/= param endparam)
            (setq blg (abs (vlax-invoke obj 'GetBulge param)))
            (/= 0 blg)
          )
          (progn
            (setq delta (* 4 (atan blg)) ;included angle
                  inc (/ 1.0 (1+ (fix (/ delta anginc))))
                  arcparam (+ param inc)
            )
            (while (< arcparam (1+ param))
              (setq pt (vlax-curve-getPointAtParam obj arcparam)
                    ptlst (cons pt ptlst)
                    arcparam (+ inc arcparam)
              )
            )
          )
        )
        (setq param (1+ param))
      )
;;;    ) ;if flag
    (if 
      (and 
        (apply 'and ptlst)
        (> (length ptlst) 1)
      )
      
     (ZClosed (reverse ptlst))
    )
  ) ;end


  ;; Argument: vla-object, an arc, circle or ellipse.
  ;; Returns: WCS point list if successful.
  (defun TraceACE (obj / startparam endparam anginc 
                         delta div inc pt ptlst)
    ;start and end angles
    ;circles don't have StartAngle and EndAngle properties.
    (setq startparam (vlax-curve-getStartParam obj)
          endparam (vlax-curve-getEndParam obj)
          anginc (* pi (/ 5.0 180.0))
    )
    (if (equal endparam (* pi 2) 1e-12)
      (setq delta endparam)
      (setq delta (NormalAngle (- endparam startparam)))
    )
    ;Divide delta (included angle) into an equal number of parts.
    (setq div (1+ (fix (/ delta anginc)))
          inc (/ delta div)
    )
    ;Or statement allows the last point on an open ellipse
    ;rather than using (<= startparam endparam) which sometimes
    ;fails to return the last point. Not sure why.
    (while
      (or
        (< startparam endparam)
        (equal startparam endparam 1e-12)
      )
      (setq pt (vlax-curve-getPointAtParam obj startparam)
            ptlst (cons pt ptlst)
            startparam (+ inc startparam)
      )
    )
    (reverse ptlst)
  ) ;end

  ;; Added because these trace functions are used with
  ;; other functions which may need this for convenience.
  (defun TraceLine (obj)
      (list
        (vlax-get obj 'StartPoint)
        (vlax-get obj 'EndPoint)
      )
  ) ;end

  ;; Note regarding the trailing part of a spline which was broken with
  ;; the break command. It's start param is not zero.
  ;; 0.0174533 one degree in radians
  ;; 0.00872665 half degree
  ;; 0.00436332 quarter degree
  ;; 0.00218166 1/8
  (defun TraceSpline (obj / startparam endparam ncpts inc param   
                            fd ptlst pt1 pt2 ang1 ang2 a)
    (setq startparam (vlax-curve-getStartParam obj)
          endparam (vlax-curve-getEndParam obj)
          ncpts (vlax-get obj 'NumberOfControlPoints)
          inc (/ (- endparam startparam) (* ncpts 7))
          param (+ inc startparam)
          fd (vlax-curve-getfirstderiv obj param)
          ptlst (cons (vlax-curve-getStartPoint obj) ptlst)
    )
 
    (while (< param endparam)
      (setq pt1 (vlax-curve-getPointAtParam obj param)
;_           ang1 (angle pt1 (mapcar '+ pt1 fd))
            ang1 fd
            param (+ param inc)
            pt2 (vlax-curve-getPointAtParam obj param)
            fd (vlax-curve-getfirstderiv obj param)
;_            ang2 (angle pt2 (mapcar '+ pt2 fd))
            ang2 fd
;_            a (abs (@delta ang1 ang2))
            a (abs (3d_angw1w2 ang1 ang2))
      )
      (if (> a 0.00218166)
        (setq ptlst (cons pt1 ptlst))
      )
    )
    ;add last point and check for duplicates
    (if 
      (not 
        (equal 
          (setq pt1 (vlax-curve-getEndPoint obj)) (car ptlst) 1e-8))
      (setq ptlst (cons pt1 ptlst))
    )
    (reverse ptlst)
  ) ;end

  ;; Explode curve fit pline and gather point list from arcs.
  ;; Checking for locked layers is the responsibility of the calling function.
  ;; This sub-function deletes objects.
  (defun TraceType1Pline (obj / ptlst objlst lst)
    (setq ptlst (list (vlax-curve-getStartPoint obj))
          objlst (vlax-invoke obj 'Explode)
    )
    (foreach x objlst 
      (setq lst (TraceACE x))
      (if (not (equal (car lst) (last ptlst) 1e-8))
        (setq lst (reverse lst))
      )
      (setq ptlst (append ptlst (cdr lst)))
      (vla-delete x)
    )
    (ZClosed  ptlst)
  ) ;end

  ;; Explode quadratic and cubic plines and gather point list from lines.
  ;; Produces an exact trace.
  ;; This sub-function deletes objects.
  (defun TraceType23Pline (obj / objlst ptlst lastpt)
    (setq objlst (vlax-invoke obj 'Explode)
          lastpt (vlax-get (last objlst) 'EndPoint)
    )
    (foreach x objlst
      (setq ptlst (cons (vlax-get x 'StartPoint) ptlst))
      (vla-delete x)
    )
   (ZClosed (reverse (cons lastpt ptlst)))
  ) ;end

  (defun Trace3DPline (obj / coord ptlst)
    (setq coord (vlax-get obj 'Coordinates))
    (repeat (/ (length coord) 3)
      (setq ptlst (cons (list (car coord) (cadr coord)(caddr coord)) ptlst))
      (setq coord (cdddr coord))
    )
    ;ZZeroList not needed here.
    (ZClosed (reverse ptlst))
  ) ;end

  ;;;; end sub-functions ;;;;

  ;;;; primary function ;;;;

  (setq typlst '("AcDb2dPolyline" "AcDbPolyline" "AcDb3dPolyline" 
                 "AcDbCircle" "AcDbArc" "AcDbEllipse" 
                 "AcDbSpline" "AcDbLine"))
  (or 
    (eq (type obj) 'VLA-OBJECT)
    (setq obj (vlax-ename->vla-object obj))
  )

  (setq typ (vlax-get obj 'ObjectName))
  
  (if (vl-position typ typlst)
    (cond
       ((or (eq typ "AcDb2dPolyline") (eq typ "AcDbPolyline")) 
          (cond
            ((or
               (not (vlax-property-available-p obj 'Type))
               (= 0 (vlax-get obj 'Type))
              )
              (TracePline obj)
            )
            ((or (= 3 (vlax-get obj 'Type)) (= 2 (vlax-get obj 'Type)))
              (TraceType23Pline obj)
            )
            ((= 1 (vlax-get obj 'Type))
              (TraceType1Pline obj)
            )
          )
       )
       ((eq typ "AcDbLine")
         (TraceLine obj)
       )
       ((or (eq typ "AcDbCircle") (eq typ "AcDbArc") (eq typ "AcDbEllipse"))
         (TraceACE obj)
       )
       ((eq typ "AcDbSpline")
         (TraceSpline obj)
       )
       ((eq typ "AcDb3dPolyline")
         (Trace3DPline obj)
       )
    )
  )
) ;end TraceObject

;;;; Toolbox Functions ;;;;

;-------------------------------------------
;; Argument: angle in radians, any number including negative.
;; Returns: normalized angle in radians between zero and (* pi 2)
(defun NormalAngle (a)
  (if (numberp a)
    (angtof (angtos a 0 14) 0))
)

; Угол между векторами (скалярное произведение векторов)
;---------------------
;Параметры:
;Wekt1, Wekt2 - вектора
;Возвращает  угол между векторами
;http://www.elecran.com.ua/index.php?pagename=programer.php
(defun 3d_angw1w2 (Wekt1 Wekt2 / CosA)
 (if (equal (setq CosA (/ (apply '+ (mapcar '* Wekt1 Wekt2))
			  (distance '(0 0 0) Wekt1) (distance '(0 0 0) Wekt2))) -1.0 1e-6)
  Pi
  (if (equal CosA 0.0 1e-6) (* 0.5 PI)(atan (sqrt (- 1 (* CosA CosA))) CosA))
 )
)
;-------------------------------------------
;; by John Uhden
;; Arguments: two angles expressed as radians.
;; Returns: the deflection angle between 0 and pi.
;; Return value may be positive or negative depending
;; on the order of the arguments passed.
(defun @delta (a1 a2)
  (cond
      ((> a1 (+ a2 pi))
        (setq a2 (+ a2 (* 2 pi)))
      )
      ((> a2 (+ a1 pi))
        (setq a1 (+ a1 (* 2 pi)))
      )
    )
    (- a2 a1)
) ;end

(defun entmakelwpline (plist flag width Elev ucszdir space)
  (entmakex
    (append
      (list
	'(0 . "LWPOLYLINE" )
	'(100 . "AcDbEntity" )
	(cons 8 (getvar "CLAYER"))
	'(100 . "AcDbPolyline" )
        (if space (cons 410 space)(cons 410 (getvar "CTAB")))
	(cons 90 (length plist))	;число вершин
	(cons 70 flag)			; флаг замкнутости
	(cons 43 width)			;ширина полилинии
        (cons 38 Elev)                  ;уровень
      )
      (mapcar '(lambda (x) (cons 10 x)) plist)
      (list(cons 210 ucszdir))
    )
  )
)

;http://www.theswamp.org/index.php?topic=11561.0
;http://www.theswamp.org/index.php?topic=13526.0
(defun ucszdir ()(trans '(0 0 1) 1 0 T))
(defun TraceToLWPlinePoint ( obj / elev )
  (vla-get-ObjectName obj)
  (mapcar '(lambda(x)(trans x  0 (ucszdir)))(TraceObject obj)))
(defun TraceToLWPlineObj ( obj / pl ptlist elev)
  (if(setq pl (entmakelwpline
      (setq ptlist (TraceToLWPlinePoint obj))
      0
      0
      (if (setq elev (caddar ptlist))
        elev
        (caddr (trans '(0 0 0) 1 (ucszdir)))
        )
      (ucszdir)
      (cdr(assoc 410 (entget(vlax-vla-object->ename obj))))
      ))
     (progn
      (setq pl (vlax-ename->vla-object pl))
      (mapcar
	   '(lambda (x y) (vlax-put-property pl x y))
	   '(Linetype LineWeight Color Layer)
	   (mapcar
	     '(lambda (x)
		(vlax-get-property obj  x))
	     '(Linetype LineWeight Color Layer)))
      pl)
     nil
     )
    )
(defun C:ConvTo2d ( / adoc isRus ret count ss)
(setq adoc (vla-get-activedocument (vlax-get-acad-object))    
     isRus (= (getvar "SysCodePage") "ANSI_1251")
      count 0)
(setq adoc (vla-get-activedocument (vlax-get-acad-object)))
(if (setq ss (ssget "_:L" '((0 . "*POLYLINE,CIRCLE,ARC,ELLIPSE,SPLINE,LINE"))))
  (progn
  ;_  (initget "Yes No")    
  ;_  (setq ret (getkword (if isRus "\nУдалять исходные объекты? [Yes/No] <No> : " "\nDelete source objects? [Yes/No] <No> : ")))
    (setq ret "Yes")
    (vla-StartUndoMark adoc)
    (foreach obj (pl:selset-to-vlalist SS)   ;_(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
       (TraceToLWPlineObj obj)
       (if (and (= ret "Yes")
                (vlax-write-enabled-p obj))
                (vla-Erase obj))
        (setq count (1+ count))
        )
    (vla-EndUndoMark adoc)
    (mapcar 'princ
    (if isRus
        (list "\nСконвертировано " count " объектов")
        (list "\nConverted " count " objects")
      )
    )
    )
    )
    (princ)
  )

(defun C:ConvTo3d ( / adoc isRus ret lst crs var pl count ss)
(setq adoc (vla-get-activedocument (vlax-get-acad-object))
     isRus (= (getvar "SysCodePage") "ANSI_1251")
      count 0)
(if (setq ss (ssget "_:L" '((0 . "*POLYLINE,CIRCLE,ARC,ELLIPSE,SPLINE,LINE"))))
  (progn
 ;_   (initget "Yes No")    
 ;_   (setq ret (getkword (if isRus "\nУдалять исходные объекты? [Yes/No] <No> : " "\nDelete source objects? [Yes/No] <No> : ")))
    (setq ret "Yes")
    (vla-StartUndoMark adoc)
    (foreach obj (mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
      (setq lst (TraceObject obj))
          (setq        
		crs (apply 'append lst)
		var (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray
		  vlax-vbDouble (cons 0 (1- (length crs)))) crs))
		)
	  (setq pl (vla-Add3DPoly (vla-ObjectIDToObject adoc (vla-get-OwnerID obj)) var))
          (mapcar
	   '(lambda (x y) (vlax-put-property pl x y))
	   '(Linetype LineWeight Color Layer)
	   (mapcar
	     '(lambda (x)
		(vlax-get-property obj  x))
	     '(Linetype LineWeight Color Layer)))
        (if (and (= ret "Yes")(vlax-write-enabled-p obj))(vla-Erase obj))
        (setq count (1+ count))
	    )
        (vla-EndUndoMark adoc)
        (mapcar 'princ
    (if isRus
        (list "\nСконвертировано " count " объектов")
        (list "\nConverted " count " objects")
      )
    )
      )
    )
    (princ)
  )

;**************** lw_width.lsp *********************************
  ;  Изменение начальной и конечной ширины
  ;  произвольного сегмента полилинии.
  ;  Автор  Евгений Елпанов.
(defun C:PL-SgWidth (/ ENDWIDTH GR LW PAR STARTWIDTH *error*)
  (vl-load-com)
  (setq *error* pltool-err)
  (or *pl-activedoc*
      (setq *pl-activedoc*
             (vla-get-activedocument (vlax-get-acad-object))
      ) ;_ end of setq
  ) ;_ end of or
  (vla-endundomark *pl-activedoc*)
  (vla-startundomark *pl-activedoc*)
  (setq lw (entsel "\n Выберите нужный сегмент в полилинии. "))
  (if (and lw (= (cdr (assoc 0 (entget (car lw)))) "LWPOLYLINE"))
    (progn (setq par (vlax-curve-getparamatpoint
                       (vlax-ename->vla-object(car lw))
                       (vlax-curve-getclosestpointto (vlax-ename->vla-object(car lw))
                                                     (trans (cadr lw) 1 0))
                     ) ;_ end of vlax-curve-getParamAtPoint
                 lw  (vlax-ename->vla-object (car lw))
           ) ;_ end of setq
           (princ "\n Задайте ширину начала сегмента: ")
           (vla-getwidth lw (fix par) 'StartWidth 'EndWidth)
           (while (and (setq gr (grread 5)) (= (car gr) 5))
             (vla-setwidth
               lw
               (fix par)
               (setq StartWidth
                      (* (distance
                           (trans (cadr gr) 1 0)
                           (vlax-curve-getclosestpointto lw (trans (cadr gr) 1 0))
                         ) ;_ end of distance
                         2.
                      ) ;_ end of *
               ) ;_ end of setq
               EndWidth
             ) ;_ end of vla-SetWidth
           ) ;_ end of while
           (if (= (car gr) 2)
             (vla-setwidth
               lw
               (fix par)
               (setq StartWidth
                      (atof (strcat (princ (vl-list->string (cdr gr)))
                                    (getstring)
                            ) ;_ end of strcat
                      ) ;_ end of atof
               ) ;_ end of setq
               EndWidth
             ) ;_ end of vla-SetWidth
           ) ;_ end of if
           (princ "\n Задайте ширину конца сегмента: ")
           (while (and (setq gr (grread 5)) (= (car gr) 5))
             (vla-setwidth
               lw
               (fix par)
               StartWidth
               (* (distance (trans (cadr gr) 1 0)
                            (vlax-curve-getclosestpointto lw (trans (cadr gr) 1 0))
                  ) ;_ end of distance
                  2.
               ) ;_ end of *
             ) ;_ end of vla-SetWidth
           ) ;_ end of while
           (if (= (car gr) 2)
             (vla-setwidth
               lw
               (fix par)
               StartWidth
               (atof
                 (strcat (princ (vl-list->string (cdr gr))) (getstring))
               ) ;_ end of atof
             ) ;_ end of vla-SetWidth
           ) ;_ end of if
    ) ;_ end of progn
    (if (= (cdr (assoc 0 (entget (car lw)))) "POLYLINE")
    (alert "Обрабатываются только легкие (LW) полилинии")  
    (princ "\n Ничего не выбрано или объект не полилиния.\t")
      )
  ) ;_ end of if
  (vla-endundomark *pl-activedoc*)
  (princ)
) ;_ end of defun


;;* Утилита объединения набора линий в полилинию*
;;------------------------------------------------
;;Алгорити взят у ChainSelect Fatty
;;http://www.cadforyou.spb.ru/index.php?current_section=section_programs_page
;;Доработан до понимания ARC,PLINE,LINE
;;Для выполнения необходимо указать только точку
;; pt - точка в мировой системе координат !!!
;;    - или имя (ENAME VLA-OBJECT) начального примитива
;; fuzz - точность
;;Возвращает список vla объектов
(defun pl:ChainSelectFromAny ( pt fuzz / chain_list couple ept line_list ln loop pda spt ss ln1 cycl)
(vl-load-com)
(cond ((= (type pt) 'ENAME)
       (setq ln (vlax-ename->vla-object pt)
             pt nil))
      ((= (type pt) 'VLA-OBJECT)(setq ln pt pt nil))
      (t nil))
(if (setq ss (ssget "_I") ss nil
          ss (ssget "_X" (list '(0 . "ARC,LINE,*POLYLINE")(cons 410 (getvar "CTAB"))))) ;_ end of setq
  (progn
    (if pt (progn
      (setq ln1 (vla-addLine
            (if (and (zerop (vla-get-activespace (vla-get-activedocument (vlax-get-acad-object))))
                     (= :vlax-false (vla-get-mspace (vla-get-activedocument (vlax-get-acad-object))))) ;_ end of and
                      (vla-get-paperspace (vla-get-activedocument (vlax-get-acad-object)))
                      (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
               (vlax-3d-point pt)(vlax-3d-point (mapcar '- pt '(1 1 0)))))
        (setq ln ln1)))
     (setq spt (vlax-curve-getStartPoint ln)  ept (vlax-curve-getEndPoint ln))
    (setq line_list  (mapcar 'vlax-ename->vla-object
                             (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                     ) ;_ end of mapcar
          chain_list nil
          chain_list (cons ln chain_list)) ;_ end of setq
    (setq line_list (vl-remove-if
                      '(lambda (x)(eq "AcDb3dPolyline" (vla-get-objectname x)))
                      line_list)) ;_ end of setq
    (setq loop t cycl 0)
    (while loop
     (while
        (setq couple
               (vl-remove-if-not
                 (function (lambda (x)
                             ;; значение допуска 0.01 можно изменить по ситуации
                             ;; в зависимости от единиц черчения : 
                             (or (equal (vlax-curve-getStartPoint x)
                                        (vlax-curve-getStartPoint ln)
                                        fuzz      ;<--- допуск 
                                 ) ;_ end of equal
                                 (equal (vlax-curve-getStartPoint x)
                                        (vlax-curve-getEndPoint ln)
                                        fuzz     ;<--- допуск 
                                 ) ;_ end of equal
                                 (equal (vlax-curve-getEndPoint x)
                                        (vlax-curve-getStartPoint ln)
                                        fuzz     ;<--- допуск 
                                 ) ;_ end of equal
                                 (equal (vlax-curve-getEndPoint x)
                                        (vlax-curve-getEndPoint ln)
                                        fuzz     ;<--- допуск 
                                 ) ;_ end of equal
                             ) ;_ end of or
                           ) ;_ end of lambda
                 ) ;_ end of function
                 line_list
               ) ;_ end of vl-remove-if-not
        ) ;_ end of setq
       (grtext -1 (strcat "Обработка. Цикл - " (itoa (setq cycl (1+ cycl)))))
         (if couple (progn
             (setq chain_list (append couple chain_list))
             (setq line_list (vl-remove ln line_list))
             (setq ln (car chain_list))) ;_ end of progn
           (setq line_list (cdr line_list))) ;_ end of if
      ) ;_ end of while
      (setq loop nil)
    ) ;_ end of while
  ) ;_ end of progn
) ;_ end of if
  (setq chain_list (vl-remove ln1 chain_list))
  (if (= (type ln1) 'VLA-OBJECT)(vl-catch-all-apply 'vla-erase (list ln1)))
  (vl-cmdf "_.redraw") chain_list)

;;;* Mark data base to allow KB:catch.
;;;* http://www.theswamp.org/index.php?topic=15863.0
(defun pl:mark (/ val)
 (setq val (getvar "cmdecho"))(setvar "cmdecho" 0)
   (if (setq *pl:mark (entlast)) nil
      (progn (entmake '((0 . "point") (10 0.0 0.0 0.0)))
             (setq *pl:mark (entlast))
             (entdel *pl:mark)))
   (setvar "cmdecho" val)(princ))
;;;* returns selection set of entities since last KB:mark.
(defun pl:get-last-ss (/ ss tmp val)
(setq val (getvar "cmdecho"))(setvar "cmdecho" 0)
(if *pl:mark (progn (setq ss (ssadd))
 (while (setq *pl:mark (entnext *pl:mark))(ssadd *pl:mark ss))
 (command "._select" ss "")(setq tmp ss ss nil));_progn
 (alert "*pl:mark not set. \n run (pl:mark) before pl:get-last-ss."));_if
 (setvar "cmdecho" val) tmp)
;; Pltools Chain Select Entities
(defun C:PL-CSE ( / ss pda en fuzz val *error*)
 (vl-load-com)
 (setq *error* pltool-err)
 (vla-StartUndoMark *pl-activedoc*)
  (setq val (getvar "cmdecho"))(setvar "cmdecho" 0)
 (if (and (setq en (car(entsel "\nВыбрать первую или последнюю линию в цепи :")))
          (wcmatch (cdr(assoc 0 (entget en))) "ARC,LINE,*POLYLINE")
          (setq en (vlax-ename->vla-object en))
          (/= "AcDb3dPolyline" (vla-get-objectname en))
          )
 (progn
 (if (null (setq fuzz (getdist "\nЗначение допуска < 0 >: ")))(setq fuzz 0))
 (setq ss (ssadd))
 (foreach item (setq lst (pl:ChainSelectFromAny en (+ fuzz 1e-6)))
      (ssadd (vlax-vla-object->ename item) ss))
  (pl:mark)
  (vl-catch-all-apply '(lambda()
  (if (setq pda (getvar "PEDITACCEPT"))(progn
    (setvar "peditaccept" 1)
    (command "_pedit" "_M" ss "" "_j" "_j" "_b" fuzz "")
    (setvar "peditaccept" pda))
    (command "_pedit" "_M" ss "" "_Y" "_j" "_j" "_b" fuzz ""))))
   (setq lst (vl-remove-if 'vlax-erased-p lst))
  (if (setq ss nil ss (pl:get-last-ss))(progn
      (if lst (foreach item lst (ssadd (vlax-vla-object->ename item) ss)))
      (setq fuzz 0)
      (while (setq en (ssname ss fuzz))
        (if (/= (cdr(assoc 0 (entget en))) "LWPOLYLINE")
          (ssdel en ss)
          (setq fuzz (1+ fuzz))))
      (sssetfirst ss ss)))
  (setq ss nil)
  )
   (princ "\nНеобходимо выбрать ОТРЕЗОК, ДУГУ, или Полилинию")
   )
(vla-EndUndoMark *pl-activedoc*)  
(setvar "cmdecho" val)(princ)
)
;(princ "\nНаберите CSS в командной строке")


(SETFUNHELP "C:PL-A2L" "pltools.chm" "PLA2L")
(SETFUNHELP "C:PL-L2A" "pltools.chm" "PLA2L")
(SETFUNHELP "C:PL-DIV" "pltools.chm" "PLDIV")
(SETFUNHELP "C:PL-VxAdd" "pltools.chm" "PLVxAdd")
(SETFUNHELP "C:ENTREVS" "pltools.chm" "ENTREVS")
(SETFUNHELP "C:ENTREV" "pltools.chm" "ENTREVS")
(SETFUNHELP "C:PL-VxRdc" "pltools.chm" "PLVXRDC")
(SETFUNHELP "C:PL-VxDel" "pltools.chm" "PLVXDEL")
(SETFUNHELP "c:PL-VxOpt" "pltools.chm" "PLVXOPT")
(SETFUNHELP "C:PL-NoArc" "pltools.chm" "PLNOARC")
(SETFUNHELP "C:ConvTo2d" "pltools.chm" "CVPOLY")
(SETFUNHELP "C:ConvTo3d" "pltools.chm" "CVPOLY")
(SETFUNHELP "C:PL-DIVALL" "pltools.chm" "PLDIVALL")
(SETFUNHELP "C:PL-CLONE" "pltools.chm" "PLCLONE")
(SETFUNHELP "C:MPL" "pltools.chm" "MPL")
(SETFUNHELP "C:R3P" "pltools.chm" "R3P")
(SETFUNHELP "C:PL-JOIN" "pltools.chm" "PLJOIN")
(SETFUNHELP "C:PL-JOIN3D" "pltools.chm" "PLJOIN3D")
(SETFUNHELP "C:PL-SgInfo" "pltools.chm" "PLSgInfo")
(SETFUNHELP "C:PL-SgWidth" "pltools.chm" "PLSgWidth")
(SETFUNHELP "C:PL-CSE" "pltools.chm" "PLCSE")


;;;Команды
(if (= (getvar "DWGCODEPAGE") "ANSI_1251")
(progn
(princ "\n=========== Определенные команды PLTOOLS=================")
(princ "\n           Редакция 28.01.2008")
(princ "\nPL-JOIN   -Объединение полилиний чохом")
(princ "\nPL-JOIN3D -Объединение 3D полилиний")
(princ "\nPL-A2L    -Замена линейного сегмента в полилинии дуговым сегментом.")
(princ "\nPL-L2A    -Замена дугового сегмента в полилинии линейным сегментом.")
(princ "\nPL-DIV    -Разбивает выбранный сегмент полилинии на указанное количество\n           сегментов или через указанное расстояние")
(princ "\nPL-DIVAll -Разбивает все сегменты полилинии на указанное количество\n           сегментов или через указанное расстояние")
(princ "\nPL-VxAdd  -Добавляет новую вершину к полилинии")
(princ "\nENTREVS   -Реверс объекта")
(princ "\nENTREV    -Реверс объектов (множественный выбор)")
(princ "\nPL-VxRdc  -Удаление вершин полилиний, которые лежат на одной прямой")
(princ "\nPL-VxDel  -Удаление выбранной вершины ")
(princ "\npl-VxOpt  -Удаление совпадающих вершин из полилинии")
(princ "\nPL-NoArc  -Аппроксимация дуговых сегментов полилинии ")
(princ "\nPL-Clone  -Построение полилинии путем копирования ее сегментов ")
(princ "\nPL-VxMove -Перемещение вершин полилинии (командная строка) ")
(princ "\nPL-Vx1    -Изменение начала полилинии (командная строка) ")
(princ "\nConvTo2d   -Преобразование линейных объектов в 2D полилинии")
(princ "\nConvTo3d   -Преобразование линейных объектов в 3D полилинии")
(princ "\nMPL        -Построение средней линии")
(princ "\nR3P        -Прямоугольгик по 3-м точкам")
(princ "\nPL-CSE     -Объединение 2d полилиний по примитиву")
(princ "\nPL-SgWidth -Изменить ширину сегмента полилинии")
;(princ "\nPL-3d2d   -Преобразование 3D полилиний в 2D полилинии")
;(princ "\nCVPOLY    -Преобразование 3D полилиний в 2D полилинии (командная строка) от Tony Tanzillo")
)
  (progn
(princ " \n =========== Certain commands PLTOOLS ================= ")
(princ " \n Edition of 28.01.2008 ")
(princ " \nPL-JOIN   -join of polylines")
(princ " \nPL-JOIN3D -join of 3D polylines")
(princ " \nPL-A2L    -replacement of a linear segment in a polyline an arc segment. ")
(princ " \nPL-L2A    -replacement of an arc segment in a polyline a linear segment. ")
(princ " \nPL-DIV    -breaks the chosen segment of a polyline into the specified quantity \n of segments or through the specified distance ")
(princ " \nPL-DIVAll -breaks all segments of a polyline into the specified quantity \n of segments or through the specified distance ")
(princ " \nPL-VxAdd  -adds new top to a polyline ")
(princ " \nENTREVS   -reverser of object ")
(princ " \nENTREV    -reverser of objects (a plural choice) ")
(princ " \nPL-VxRdc  -removal of tops of polylines which lay on one straight line ")
(princ " \nPL-VxDel  -removal of the chosen top ")
(princ " \npl-VxOpt  -removal of conterminous tops from a polyline ")
(princ " \nPL-NoArc  -approximation of arc segments of a polyline ")
(princ " \nPL-Clone  -Construction of a polyline by copying segments of a polyline ")
(princ " \nPL-VxMove -moving of tops of a polyline (a command line) ")
(princ " \nPL-Vx1    -change of the beginning of a polyline (a command line) ")
(princ " \nConvTo2d  -transformation of linear objects in 2D polylines ")
(princ " \nConvTo3d  -transformation of linear objects in 3D polylines ")
(princ " \nMPL       -construction of an average line ")
(princ " \nR3P       -rectangle on 3-rd points ")
)
)
(princ)
(princ " ... ok")
