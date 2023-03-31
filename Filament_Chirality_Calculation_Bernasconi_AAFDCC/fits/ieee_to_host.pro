pro ieee_to_host, data, IDLTYPE = idltype
;+
; NAME:
;	IEEE_TO_HOST
; PURPOSE:
;	Translate an IDL variable from IEEE-754 to host representation 
; EXPLANATION:
;	The variable is translated from IEEE-754 (as used, for
;	example, in FITS data ), into the host machine architecture.
;
; CALLING SEQUENCE:
;	IEEE_TO_HOST, data, [ IDLTYPE = , ]
;
; INPUT-OUTPUT PARAMETERS:
;	data - any IDL variable, scalar or vector.   It will be modified by
;		IEEE_TO_HOST to convert from IEEE to host representation.  Byte 
;		and string variables are returned by IEEE_TO_HOST unchanged
;
; OPTIONAL KEYWORD INPUTS:
;	IDLTYPE - scalar integer (1-9) specifying the IDL datatype according
;		to the code given by the SIZE function.     This keyword
;		is usually when DATA is a byte array to be interpreted as
;		another datatype (e.g. FLOAT).
;
; EXAMPLE:
;	A 2880 byte array (named FITARR) from a FITS record is to be 
;	interpreted as floating and converted to the host representaton:
;
;	IDL> IEEE_TO_HOST, fitarr, IDLTYPE = 4     
;
; METHOD:
;	The BYTEORDER procedure is called with the appropriate keyword
;
; RESTRICTION:
;	Assumes the IDL version is since 2.2.2 when the /XDRTOF keyword 
;	became available to BYTEORDER.    There were two bad implementations
;	in BYTEORDER for double precision: (1) in IDL V3.* for DecStations
;	(!VERSION.ARCH = 'mipsel') and (2) on Dec Alpha OSF machines prior to
;	IDL V4.0.1b.   IEEE_TO_HOST works around these cases by swapping bytes 
;	directly.
;
; MODIFICATION HISTORY:
;	Written, W. Landsman   Hughes/STX   May, 1992
;	Fixed error Case statement for float and double   September 1992
;	Workaround to /XDRTOD problem on DecStations January 1993 
;	Assume since Version 2.2, correct double precision problems in 
;	Alpha/OSF, implement Ultrix corrections from Tom McGlynn November 1994
;	Added support for double precision complex   July 1995
;	Workaround for BYTEORDER, /FTOXDR bug in VAX VMS V4.0  August 1995 
;	Alpha/OSF problems fixed after V4.0.1b    August 1997
;	Don't change double scalars to 1 element arrays November 1997
;	Under VMS check for IEEE -0.0 values   January 1998
;-
 On_error,2 

 if N_params() EQ 0 then begin
    print,'Syntax - IEEE_TO_HOST, data, [ IDLTYPE = ]
    return
 endif  
 

 npts = N_elements( data )
 if npts EQ 0 then $
     message,'ERROR - IDL data variable (first parameter) not defined'

 sz = size(data)
 if not keyword_set( idltype) then idltype = sz( sz(0)+1)

; Check for IEEE negative zero values which VMS can't handle

 chk_negzero = (!VERSION.OS EQ 'vms') and ((idltype EQ 4) or (idltype EQ 5) $
                or (idltype EQ 6) or (idltype EQ 9))
 chk_negzero = chk_negzero and (!VERSION.RELEASE LT 5.1)               
 if chk_negzero then wn = WHERE_NEGZERO( data, NCount, /QUIET)

 case idltype of

      1: return                             ;byte

      2: byteorder, data, /NTOHS            ;integer

      3: byteorder, data, /NTOHL            ;long

      4: begin                              ;float

         if (!VERSION.ARCH EQ 'vax') and (!VERSION.RELEASE EQ '4.0')  then $
         	         conv_unix_vax, data, source='sparc' $
		    else byteorder, data, /XDRTOF          
         end

      5: BEGIN                              ;double
       
     bad_ultrix =   (!VERSION.ARCH EQ 'mipsel') and $
                    strmid(!VERSION.RELEASE,0,1) EQ '3'
     bad_osf  =     (!VERSION.ARCH EQ 'alpha') and (!VERSION.OS EQ 'OSF') and $
                    (!VERSION.RELEASE LT '4.0.1b')

     if bad_ultrix or bad_osf then begin

                    dtype = sz( sz(0) + 1)
                    if ( dtype EQ 5 ) then data = byte(data, 0, npts*8) $
                                      else npts = npts/8
                    data = reform( data, 8 , npts ,/OVER)
                    data = rotate( data, 5)
                    if ( dtype EQ 5 ) then data = double(data, 0, npts)
                    if sz(0) gt 0 then data = reform( data, sz(1:sz(0)), /OVER)$
			else data = data(0)
      endif else byteorder, data, /XDRTOD

      end
     
      6: byteorder, data, /XDRTOF

      7: return                             ;string

       8: BEGIN				    ;structure

	Ntag = N_tags( data )

	for t=0,Ntag-1 do  begin
          temp = data.(t)
          ieee_to_host, temp
          data.(t) = temp
        endfor 

       END

	9: byteorder, data,/XDRTOD                              ;complex

 ENDCASE

 if chk_negzero then if NCount GT 0 then data(wn) = 0

 return
 end 
