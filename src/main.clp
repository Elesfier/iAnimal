
;;
;;	( Comments Language (PL) Poland )
;;
;;	Zwykły program służacy do znalezienia zwierzęcia przez
;;	podawaniu kilku cech o nim.
;;	Author: Artur Szajdecki <artur.szajdecki@gmail.com>
;;

; (load "/home/elesfier/Documents/Studia/NiMSI/iAnimal/src/main.clp")
; (load "/home/elesfier/Documents/Studia/NiMSI/iAnimal/src/animals.clp")

; Zmienna globalna która prezentuje liczbę dostępnych zwierząt
(defglobal ?*animalNumber* = 0)

; Zmienne globalne pokazujace ktore wartosci sa mozliwe wy wyborze
(defglobal
	?*sizeValues* = (create$ malutkie małe średnie duże)

	?*colorValues* = (create$ brązowe żółte czarne szare zielone czerwone białe
	niebieskie)

	?*spotsValues* = (create$ tak nie)

	?*bodyValues* = (create$ duże-oczy futro paski długi kolczasty ostry
	macki ogon płetwa rogi wąsy duży-grzbiet wielenóg trąba pancerz)

	?*environmentValues* = (create$ woda plaża jaskinia krzaki las bagno
	plony podwodą kamienie góry pola pustynia liście safari)

	?*particularityValues* = (create$ samotnik może-nurkować niebezpieczny może-pływać
	zatruwa tryb-nocny skacze lata kamuflowanie stada roślinożerny)
)

; (animal
;   (name ?STRING)
;   (kind ?STRING)
;   (size ?SYMBOL)
;   (colors $?SYMBOL)
;   (spots SYMBOL)
;   (body $?SYMBOL)
;   (environment $?SYMBOL)
;   (particularity $?SYMBOL)
; )
(deftemplate animal "Szablon zwierzaka"

	; Nie ma zadnego ograniczenia z allowed-symbols

	; Nazwa
	(multislot name (type STRING) (default ?NONE))

	; Rodzaj
	(multislot kind (type STRING) (default ?NONE))

	; Rozmiar
	(slot size (type SYMBOL) (default ?NONE))

	; Kolor.
	(multislot colors (type SYMBOL) (default ?NONE))

	; Plamki
	(slot spots (type SYMBOL) (default nie))

	; Ciało
	(multislot body (type SYMBOL) (default ?NONE))

	; Środowisko
	(multislot environment (type SYMBOL) (default ?NONE))

	; Szczegóły, dodatkowe cecha
	(multislot particularity (type SYMBOL) (default ?NONE))
)

; Reguła która zlicza ile jest zwierząt w bazie danych
(defrule count-animals
	(animal (name ?n)) => (bind ?*animalNumber* (+ ?*animalNumber* 1))
)

; Dekrementuje liczbę dostępnych zwierząt
(deffunction decrementAnimalNumber ()
	(bind ?*animalNumber* (- ?*animalNumber* 1))
)

; Funkcja pomocnicza w zadawaniu pytan
(deffunction ask-a-question (?question $?allowed-values)
	(printout t ?question)
	(bind ?answer (read))
	(if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
	(while (not (member ?answer ?allowed-values)) do
		(printout t ?question)
		(bind ?answer (read))
		(if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
	)
?answer)

; Reguła kasujące niepasujące zwierzeta - wielkosc
(defrule size-filter
	(sizeOfAnimal ?v)
	?ani <- (animal (size ?s) (name ?n))
	=>
	(if (not (eq ?s ?v)) then
		(retract ?ani)
		(decrementAnimalNumber)
	)
)

; Pytanie dotyczące rozmiaru zwierzecia
(defrule size-question
	?startRules <- (initial-fact)
	=>
	(retract ?startRules)
	(printout t "Start programu." crlf)
	(bind ?size (ask-a-question
		"Jak duże jest zwierzę?: "
		$?*sizeValues*)
	)
	(assert (sizeOfAnimal ?size))
)

; Reguła kasujące niepasujące zwierzeta - kolory
(defrule color-filter
	(colorOfAnimal ?c)
	?ani <- (animal (colors $?value) (name ?n))
	=>
	(if (not (member ?c $?value)) then
		(retract ?ani)
		(decrementAnimalNumber)
	)
)

; Pytanie dotyczące koloru zwierzecia
(defrule color-question
	(sizeOfAnimal ?size)
	=>
	(bind ?color (ask-a-question
		"Jaki kolor ma zwierzę?: "
		$?*colorValues*))
	(assert (continue replay))
	(assert (colorOfAnimal ?color))
)

; Reguła kasujące niepasujące zwierzeta - plamy
(defrule spots-filter
	(ask-about-spots ?v)
	?ani <- (animal (spots ?s))
	=>
	(if (not (eq ?s ?v)) then
		(retract ?ani)
		(decrementAnimalNumber)
	)
)

; Reguła kasujące niepasujące zwierzeta - ciało
(defrule body-filter
	(ask-about-body ?v)
	?ani <- (animal (body $?values) (name ?n))
	=>
	(if (not (member ?v $?values)) then (retract ?ani) (decrementAnimalNumber))
)

; Reguła kasujące niepasujące zwierzeta - środowisko
(defrule environment-filter
	(ask-about-environment ?v)
	?ani <- (animal (environment $?values))
	=>
	(if (not (member ?v $?values)) then (retract ?ani) (decrementAnimalNumber))
)

; Reguła kasujące niepasujące zwierzeta - szczegóły
(defrule particularity-filter
	(ask-about-particularity ?v)
	?ani <- (animal (particularity $?values))
	=>
	(if (not (member ?v $?values)) then (retract ?ani) (decrementAnimalNumber))
)

; Menu z opcjami
(defrule menu
	?con <- (continue ?replay)
	=>
	(retract ?con)
	(printout t crlf)
	(printout t "Zwierzat znalezionych po podaniu cech: " ?*animalNumber* crlf)
	(bind ?option (ask-a-question
		"Jakie rodzaj pytania zadać (spot, envi, body, part, end) ?: "
		spot envi end part body)
	)
	(switch ?option
		(case spot then
a
			(bind ?answer (ask-a-question
				"Czy zwierzak ma plamki?: "
				$?*spotsValues*)
			)
			(assert (continue replay))
			(assert (ask-about-spots ?answer))
		)
		(case body then

			(bind ?answer (ask-a-question
				"Część ciała zwierzęcia?: "
				$?*bodyValues*)
			)
			(assert (continue replay))
			(assert (ask-about-body ?answer))

		)
		(case envi then

			(bind ?answer (ask-a-question
				"W jakim środowisku zwierzę przebywa?: "
				$?*environmentValues*)
			)
			(assert (continue replay))
			(assert (ask-about-environment ?answer))

		)
		(case part then

			(bind ?answer (ask-a-question
				"Czy zwierzę ma jakieś dodatkowe cechy?: "
				$?*particularityValues*)
			)
			(assert (continue replay))
			(assert (ask-about-particularity ?answer))

		)
		(case end then

			(printout t crlf)
			(printout t "Kończenie programu." crlf)
			(printout t "Liczba znalezionych zwierzat: " ?*animalNumber* crlf)
			(printout t crlf)

			(if (>= ?*animalNumber* 1) then
					(assert (found true))
			else (if (eq ?*animalNumber* 0) then
			 		(assert (found false)))
			)

		)
	)
)

; Reguła wyświetlająca znalezione zwierzęta
(defrule found
	?f <- (found true)
	?ani <- (animal (name ?n) (kind ?k))
	=>
	(retract ?ani)
	(printout t " [Nazwa: \"" ?n "\" Rodzaj: \"" ?k "\" ] " crlf)
)

; Reguła wyświetlająca nie znalezione zwierzęta
(defrule found-not
	?f <- (found false)
	=>
	(retract ?f)
	(printout t "============================" crlf)
	(printout t " Nie znaleziono zwierzęcia!" crlf)
	(printout t "============================" crlf)
	(printout t crlf)
)
