;+
; NAME:
;       FIND_BOUNDARY
;
; PURPOSE:
;
;       This program finds the boundary points about a region of interest (ROI)
;       represented by pixel indices. It uses a "chain-code" algorithm for
;       finding the boundary pixels.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics, math.
;
; CALLING SEQUENCE:
;
;       boundaryPts = Find_Boundary(indices, XSize=xsize, YSize=ysize)
;
; OPTIONAL INPUTS:
;
;       indices - A 1D vector of pixel indices that describe the ROI. For
;            example, the indices may be returned as a result of the WHERE
;            function.
;
; OUTPUTS:
;
;       boundaryPts - A 2-by-n points array of the X and Y points that
;            describe the boundary. The points are scaled if the SCALE
;            keyword is used.
;
; INPUT KEYWORDS:
;
;       SCALE - A one-element or two-element array of the pixel scale
;            factors, [xscale, yscale], used to calculate the perimeter
;            length or area of the ROI. The SCALE keyword is NOT applied to
;            the boundary points. By default, SCALE=[1,1].
;
;       XSIZE - The X size of the window or array from which the ROI
;               indices are taken. Set to !D.X_Size by default.
;
;       YSIZE - The Y size of the window or array from which the ROI
;               indices are taken. Set to !D.Y_Size by default.
;
; OUTPUT KEYWORDS:
;
;       AREA - A named variable that contains the pixel area represented by
;              the input pixel indices, scaled by the SCALE factors.
;
;       CENTER - A named variable that contains a two-element array
;                containing the center point or
;                centroid of the ROI. The centroid is the position in the ROI
;                that the ROI would balance on if all the index pixels were
;                equally weighted. The output is a two-element
;                floating-point array in device coordinate system, unless
;                the SCALE keyword is used, in which case the values will
;                be in the scaled coordinate system.
;
;       PERIM_AREA - A named variable that contains the (scaled) area
;                    represented by the perimeter points, as indicated by
;                    John Russ in _The Image Processing Handbook,
;                    2nd Edition_ on page 490. This is the same "perimeter"
;                    that is returned by IDLanROI in its ComputeGeometry
;                    method, for example. In general, the perimeter area
;                    will be smaller than the pixel area.
;
;       PERIMETER - A named variable that will contain the perimeter length
;                   of the boundary upon returning from the function,
;                   scaled by the SCALE factors.
;
;  EXAMPLE:
;
;       LoadCT, 0, /Silent
;       image = BytArr(400, 300)+125
;       image[125:175, 180:245] = 255B
;       indices = Where(image EQ 255)
;       Window, XSize=400, YSize=300
;       TV, image
;       PLOTS, Find_Boundary(indices, XSize=400, YSize=300, Perimeter=length),$
;           /Device, Color=FSC_Color('red')
;       Print, length
;           230.0
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, April 2002. Based on an algorithm
;       written by Guy Blanchard and provided by Richard Adams.
;       Fixed a problem with distinction between solitary points and
;          isolated points (a single point connected on a diagonal to
;          the rest of the mask) in which the program can't get back to
;          the starting pixel. 2 Nov 2002. DWF
;       Added the ability to return the perimeter length with PERIMETER and
;           SCALE keywords. 2 Nov 2002. DWF.
;       Added AREA keyword to return area enclosed by boundary.2 Nov 2002. DWF.
;       Fixed a problem with POLYFILLV under-reporting the area by removing
;           POLYFILLV and using a pixel counting method. 10 Dec 2002. DWF.
;       Added the PERIM_AREA and CENTER keywords. 15 December 2002. DWF.
;       Replaced the ERROR_MESSAGE routine with the latest version.
;           15 December 2002. DWF.
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 1997-2002 Fanning Software Consulting.
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################


;******************************************************************************
;******************************************************************************
FUNCTION Find_Boundary_ERROR_MESSAGE, theMessage, Error=error,$
   Informational=information, Traceback=traceback, NoName=noname,$
   Title=title, _Extra=extra

On_Error, 2

   ; Check for presence and type of message.

IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
s = Size(theMessage)
messageType = s[s[0]+1]
IF messageType NE 7 THEN BEGIN
   Message, "The message parameter must be a string.", _Extra=extra
ENDIF

   ; Get the call stack and the calling routine's name.

Help, Calls=callStack
IF Float(!Version.Release) GE 5.2 THEN $
  callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0] $
ELSE $
  callingRoutine = (Str_Sep(StrCompress(callStack[1])," "))[0]

   ; Are widgets supported?

widgetsSupported = ((!D.Flags AND 65536L) NE 0)
IF widgetsSupported THEN BEGIN

      ; If this is an error produced with the MESSAGE command, it is a trapped
      ; error and will have the name "IDL_M_USER_ERR".

   IF !ERROR_STATE.NAME EQ "IDL_M_USER_ERR" THEN BEGIN

      IF N_Elements(title) EQ 0 THEN title = 'Trapped Error'

         ; If the message has the name of the calling routine in it,
         ; it should be stripped out. Can you find a colon in the string?

      colon = StrPos(theMessage, ":")
      IF colon NE -1 THEN BEGIN

            ; Extract the text up to the colon. Is this the same as
            ; the callingRoutine? If so, strip it.

         IF StrMid(theMessage, 0, colon) EQ callingRoutine THEN $
            theMessage = StrMid(theMessage, colon+1)

      ENDIF

         ; Add the calling routine's name, unless NONAME is set.

      IF Keyword_Set(noname) THEN BEGIN
         answer = Dialog_Message(theMessage, Title=title, _Extra=extra, $
            Error=error, Information=information)
      ENDIF ELSE BEGIN
         answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + $
            theMessage, Title=title, _Extra=extra, $
            Error=error, Information=information)
      ENDELSE

   ENDIF ELSE BEGIN

         ; Otherwise, this is an IDL system error.

      IF N_Elements(title) EQ 0 THEN title = 'System Error'

      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN $
         answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
            Error=error, Information=information) ELSE $
      IF Keyword_Set(noname) THEN BEGIN
         answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
            Error=error, Information=information)
      ENDIF ELSE BEGIN
         answer = Dialog_Message(StrUpCase(callingRoutine) + "--> " + $
            theMessage, _Extra=extra, Title=title, $
            Error=error, Information=information)
      ENDELSE
   ENDELSE
ENDIF ELSE BEGIN
      Message, theMessage,/Continue, /NoPrint, /NoName, /NoPrefix, _Extra=extra
      Print, '%' + callingRoutine + ': ' + theMessage
      answer = 'OK'
ENDELSE

   ; Provide traceback information if requested.

IF Keyword_Set(traceback) THEN BEGIN
   Help, /Last_Message, Output=traceback
   Print,''
   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
   Print, ''
   FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
ENDIF

RETURN, answer
END
;------------------------------------------------------------------------------



;******************************************************************************
;******************************************************************************
FUNCTION Find_Boundary_Outline, mask, darray, boundaryPts, ptIndex, $
   xsize, ysize, from_direction

FOR j=1,7 DO BEGIN

   to_direction = (from_direction + j) MOD 8
   newPt = boundaryPts[*,ptIndex-1] + darray[*,to_direction]

      ; If this is the edge, assume it is a background point.

   IF (newpt[0] LT 0 OR newpt[0] GE xsize OR newpt[1] LT 0 OR $
       newpt[1] GE ysize) THEN CONTINUE

   IF mask[newPt[0], newPt[1]] NE 0 THEN BEGIN
      boundaryPts[*,ptIndex] = newPt

         ; Return the "from" direction.

      RETURN, (to_direction + 4) MOD 8
   ENDIF

ENDFOR

   ; If we get this far, this is either a solitary point or an isolated point.

IF TOTAL(mask GT 0) GT 1 THEN BEGIN ; Isolated point.
   newPt = boundaryPts[*,ptIndex-1] + darray[*,from_direction]
   boundaryPts[*,ptIndex] = newPt
   RETURN, (from_direction + 4) MOD 8
ENDIF ELSE BEGIN ; Solitary point.
   boundaryPts[*,ptIndex] = boundaryPts[*,ptIndex-1]
   RETURN, -1
ENDELSE
END
;------------------------------------------------------------------------------


;******************************************************************************
;******************************************************************************
FUNCTION Find_Boundary, indices, $
   AREA=area, $
   CENTER=center, $
   PERIM_AREA=perim_area, $
   PERIMETER=perimeter, $
   SCALE=scale, $
   XSIZE=xsize, $
   YSIZE=ysize


Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Find_Boundary_Error_Message(/Traceback)
   RETURN, -1
ENDIF


IF N_Elements(indices) EQ 0 THEN $
  Message, 'Indices of boundary region are required. Returning...'
IF N_Elements(scale) EQ 0 THEN BEGIN
   diagonal = SQRT(2.0D)
ENDIF ELSE BEGIN
   scale = Double(scale)
   diagonal = SQRT(scale[0]^2 + scale[1]^2)
ENDELSE
IF N_Elements(xsize) EQ 0 THEN xsize = !D.X_Size
IF N_Elements(ysize) EQ 0 THEN ysize = !D.Y_Size
IF Arg_Present(perimeter) THEN perimeter = 0.0

   ; Create a mask with boundary region inside.

indices = indices[Uniq(indices)]
mask = BytArr(xsize, ysize)
mask[indices] = 255B

   ; Set up a direction array.

darray = [[1,0],[1,1],[0,1],[-1,1],[-1,0],[-1,-1],[0,-1],[1,-1]]

   ; Find a starting point. The pixel to the left of
   ; this point is background

i = Where(mask GT 0)
firstPt = [i[0] MOD xsize, i[0] / xsize]
from_direction = 4

   ; Set up output points array.

boundaryPts = IntArr(2, Long(xsize) * ysize / 4L)
boundaryPts[0] = firstPt
ptIndex = 0L

   ;   We shall not cease from exploration
   ;   And the end of all our exploring
   ;   Will be to arrive where we started
   ;   And know the place for the first time
   ;
   ;                     T.S. Eliot

REPEAT BEGIN

   ptIndex = ptIndex + 1L
   from_direction = Find_Boundary_Outline(mask, darray, $
      boundaryPts, ptIndex, xsize, ysize, from_direction)

   IF N_Elements(perimeter) NE 0 THEN BEGIN
      IF N_Elements(scale) EQ 0 THEN BEGIN
         CASE from_direction OF
            0: perimeter = perimeter + 1.0D
            1: perimeter = perimeter + diagonal
            2: perimeter = perimeter + 1.0D
            3: perimeter = perimeter + diagonal
            4: perimeter = perimeter + 1.0D
            5: perimeter = perimeter + diagonal
            6: perimeter = perimeter + 1.0D
            7: perimeter = perimeter + diagonal
            ELSE: perimeter = 4
         ENDCASE
       ENDIF ELSE BEGIN
         CASE from_direction OF
            0: perimeter = perimeter + scale[0]
            1: perimeter = perimeter + diagonal
            2: perimeter = perimeter + scale[1]
            3: perimeter = perimeter + diagonal
            4: perimeter = perimeter + scale[0]
            5: perimeter = perimeter + diagonal
            6: perimeter = perimeter + scale[1]
            7: perimeter = perimeter + diagonal
            ELSE: perimeter = (2*scale[0]) + (2*scale[1])
         ENDCASE
      ENDELSE
   ENDIF

ENDREP UNTIL (boundaryPts[0,ptIndex] EQ firstPt[0] AND $
            boundaryPts[1,ptIndex] EQ firstPt[1])

boundaryPts = boundaryPts[*,0:ptIndex-1]

   ; Calculate area.

IF N_Elements(scale) EQ 0 THEN BEGIN

   area = N_Elements(i)

      ; Calculate area from the perimeter.
      ; The first point must be the same as the last point. Method
      ; of Russ, p.490, _Image Processing Handbook, 2nd Edition_.

   bx = Double(Reform(boundaryPts[0,*]))
   by = Double(Reform(boundaryPts[1,*]))
   bx = [bx, bx[0]]
   by = [by, by[0]]
   n = N_Elements(bx)
   perim_area = Total( (bx[1:n-1] + bx[0:n-2]) * (by[1:n-1] - by[0:n-2]) ) / 2.


ENDIF ELSE BEGIN

   area = N_Elements(i) * scale[0] * scale[1]

      ; Calculate area from the perimeter.
      ; The first point must be the same as the last point. Method
      ; of Russ, p.490, _Image Processing Handbook, 2nd Edition_.

   bx = Double(Reform(boundaryPts[0,*])) * scale[0]
   by = Double(Reform(boundaryPts[1,*])) * scale[1]
   bx = [bx, bx[0]]
   by = [by, by[0]]
   n = N_Elements(bx)
   perim_area = Total( (bx[1:n-1] + bx[0:n-2]) * (by[1:n-1] - by[0:n-2]) ) / 2.

   boundaryPts = Double(Temporary(boundaryPts))
   boundaryPts[0,*] = boundaryPts[0,*] * scale[0]
   boundaryPts[1,*] = boundaryPts[1,*] * scale[1]
ENDELSE

   ; Calculate the centroid

mask = mask GT 0
totalMass = Total(mask)
xcm = Total( Total(mask, 2) * Indgen(xsize) ) / totalMass
ycm = Total( Total(mask, 1) * Indgen(ysize) ) / totalMass
center = [xcm, ycm]

RETURN, boundaryPts
END
;------------------------------------------------------------------------------
