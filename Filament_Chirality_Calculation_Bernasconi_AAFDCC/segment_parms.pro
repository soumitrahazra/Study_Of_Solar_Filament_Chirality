;******************************************************************************
;***                                                                        ***
      function segment_parms, x1, y1, x2, y2
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    SEGMENT_PARMS
;
; PURPOSE:
;    Calculates the slope (a)  and the y-intercept (b) of a segment, by
;    assuming that the segment is part of a line with equation: y = a*x + b
;
; CALLING SEQUENCE:
;    parms = segment_parms(x1, y1, x2, y2)
;
; INPUTS:
;    x1,y1 = float or int: X & Y coordinates of first point in segment
;    x2,y2 = float or int: X & Y coordinates of second point in segment
;
; KEYWORD PARAMETERS:
;    None
;
; OUTPUTS:
;    parms = dblarr(2): 0 -> slope, 1 -> y-intercept
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 4) then $
  message,'syntax: parms = segment_parms(x1, y1, x2, y2)'

out = dblarr(2)

denom = double(x2-x1)
if (denom eq 0) then denom = double(0.0000000000001)

;--- first element is the slope:
out(0) = double(y2-y1) / denom

;--- second element is the intercept:
out(1) = y1 - out(0) * x1

return, out
end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: segment_parms.pro,v $
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.1  2004/08/24 20:21:18  pietro
; Fixed small syntax bug that was not affecting the performance
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.3  2004/06/11 20:12:31  pietro
; Added some documentation in header
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL: February 23, 2004
