;
; $Id: avg.pro,v 2.0 2004/06/16 21:23:12 pietro Exp $
;
FUNCTION AVG,ARRAY,DIMENSION

; NAME:
;	AVG
; PURPOSE:
;	Calculate the average value of an array, or calculate the average
;	value over one dimension of an array as a function of all the other
;	dimensions.

ON_ERROR,2
S = SIZE(ARRAY)
IF S(0) EQ 0 THEN BEGIN
	PRINT,'*** Variable must be an array, name= ARRAY, routine AVG.'
	RETURN,ARRAY
ENDIF
IF N_PARAMS() EQ 1 THEN BEGIN
	AVERAGE = TOTAL(ARRAY) / N_ELEMENTS(ARRAY)
END ELSE BEGIN
	IF ((DIMENSION GE 0) AND (DIMENSION LT S(0))) THEN BEGIN
		AVERAGE = SUM(ARRAY,DIMENSION) / S(DIMENSION+1)
	END ELSE BEGIN
		PRINT,'*** Dimension out of range, name= ARRAY, routine AVG.'
		RETURN,ARRAY
	ENDELSE
ENDELSE
;
RETURN,AVERAGE
END
