;******************************************************************************
;***                                                                        ***
      function distance, x1, y1, x2, y2
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    DISTANCE
;
; PURPOSE:
;    Calculate the distance from points (x1,y1) to (x2,y2)
;
; CALLING SEQUENCE:
;    result = distance(x1, y1, x2, y2)
;
; INPUTS:
;    x1,y1 = float or int: coordinates of first point
;    x2,y2 = floatarr(npts) or intarr(npts): coordinates of second point
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    None
;
; OUTPUTS:
;    result = doublearr(npts): the distance between (x1,y1) and (x2,y2)
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; PROCEDURE:
;    dist = sqrt(delta_x^2+delta_y^2)
;
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 4) then $
  message,'Syntax: dist = distance(x1,y1,x2,y2)'

xdiff = double(x2-x1)
ydiff = double(y2-y1)
d = sqrt(xdiff^2 + ydiff^2)

return, d

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: distance.pro,v $
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.2  2004/08/24 17:44:19  pietro
; Fixed little syntax bug
;
; Revision 2.1  2004/06/24 15:14:00  pietro
; Added some more documentation in header
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL: February 12, 2003
