;******************************************************************************
;***                                                                        ***
      pro plot_cross, x, y, color=color, length=length, thick=thick
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    PLOT_CROSS
;
; PURPOSE:
;    This procedure plots a cross at aspecific position in a plot window
;
; CALLING SEQUENCE:
;    plot_cross, x, y [, COLOR=olor, LENGTH=length, THICK=thick]
;
; INPUTS:
;    x, y= floats: coordinates in DEVICE units where the cross will be plotted.
;
; KEYWORD PARAMETERS:
;    COLOR  = color code
;    LENGTH = lenght of the cross arms. Default = 4
;    THICK = thickness of line
;
; OUTPUTS:
;	None
;
; PROCEDURES USED:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 2) then $
  message,'syntax: plot_cross, x, y [, COLOR=color, LENGTH=length,'+$
    ' THICK=thick]'

if (n_elements(length) le 0) then length = 4

arr = indgen(2,3)
arr[0,1] = x
arr[1,1] = y
arr[0,0] = arr[0,1]-length
arr[1,0] = arr[1,1]
arr[0,2] = arr[0,1]+length
arr[1,2] = arr[1,1]
plots, arr, /Device, Color=color,thick=thick
arr[0,0] = arr[0,1]
arr[1,0] = arr[1,1]-length
arr[0,2] = arr[0,1]
arr[1,2] = arr[1,1]+length
plots, arr, /Device, Color=color,thick=thick

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: plot_cross.pro,v $
; Revision 3.1  2004/11/19 22:39:30  pietro
; Apparently some changes have been done to the routine
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.1  2004/06/21 17:44:58  pietro
; Now the color input is the standard input as with plot, etc.
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL: February 23, 2004
