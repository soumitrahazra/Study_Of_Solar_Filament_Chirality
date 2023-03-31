;******************************************************************************
;***                                                                        ***
      function read_input,infile,VERBOSE=verbose
;***                                                                        ***
;******************************************************************************
;+
; PROJECT: Automated Solar Filament Detection and Characterization
;
; NAME:
;    READ_INPUT
;
; PURPOSE:
;    This routine reads the input for the routine solar_filaments from an
;    ASCII file. The results are put into a structure
;
; CALLING SEQUENCE:
;    parms = read_input(infile [, /VERBOSE])
;
; INPUTS:
;    infile = string: ASCII file with the input parameters. See the exampe
;                     input file 'template.in'
;
; OUTPUTS:
;    parms = structure:
;       ** Structure PARAMETERS, 13 tags, length=48:
;          IMAGE           STRING    'image.fits'
;          IN_PATH         STRING    'in_path'
;          OUT_PATH        STRING    'out_path'
;          VOE_PATH        STRING    'voevent_path'
;          I_THRSH         INT           -650
;          U_THRSH         INT           -650
;          MIN_AREA        INT            500
;          DISP_DIM        FLOAT          1.0
;          STARTX          INT              0
;          STARTY          INT              0
;          WIDTH           INT              0
;          HEIGHT          INT              0
;          SMTH_IMG        INT              3
;          DO_VOE          INT              0
;          VOE_UPDT        INT              0
;          DO_TRACK        INT              0
;          FIX_BGR         INT              0
;          SHOW_PIX        INT              0
;          SHOW_BBX        INT              0
;          SHOW_BND        INT              0
;          SHOW_SPN        INT              0
;          SAVE_TAB        INT              0
;          SAVE_MSK        INT              0
;          SAVE_PNG        INT              0
;          SAVE_FIL        INT              0
;          SAVE_DAT        INT              0
;
; KEYWORDS:
;    VERBOSE = flag: If set then prints the parameters read from input file
;
; ROUTINES CALLED:
;    None
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 1) then message, 'Syntax: result = read_input(<infile>)'

;--- Opens file:
openr,unit,infile,/get_lun, ERROR=err
if (err ne 0) then begin
    print,'% READ_INPUT: ',!ERR_STRING
    return, -1
endif

;--- Initializing structure with parameters:
p={parameters, image:'', in_path:'', out_path:'', voe_path:'', $
   mask_deg:75.0, i_thrsh:-650, u_thrsh:-650, $
   min_area:250, disp_dim:1., startx:0, starty:0, width:0, height:0, $
   smth_img:3, barb_ang: 0.6, max_dist: 25, fix_bgr:0, show_pix:0, show_bbx:0,$
   show_bnd:0, show_spn:0, do_voe:0, voe_update:0, $
   do_track:0, save_tab:0, save_msk:0, save_png:0, save_fil:0, save_dat:0}

s=''
image_found = 0

u_thr_set=0

;--- new parms from input file:
while (EOF(unit) le 0) do begin
    readf,unit,s
    if ((strpos(s,';*') ne 0) and (s ne '')) then begin
        param = strupcase(strmid(s, 0, 8))

        ;--- for IMAGE parameter:
        if strpos(param, 'IMAGE') ge 0 then begin
            image_found=1
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.image = strtrim(strmid(s,9,last),2)

            ;--- checking if a path has been added to the file name
            repeat begin
                pp = strpos(p.image,'/')
                if (pp ge 0) then begin
                    p.in_path  = p.in_path+strmid(p.image,0,pp+1)
                    p.image = strmid(p.image,pp+1)
                endif
            endrep until(pp lt 0)
            p.out_path = p.in_path
        endif

        ;--- for IN_PATH parameter:
        if (strlen(p.in_path) le 0) and $
          strpos(param, 'IN_PATH') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.in_path = strtrim(strmid(s,9,last),2)
            ll = strlen(p.in_path)
            if ((ll ne 0) and (strmid(p.in_path,ll-1) ne '/')) then $
              p.in_path = p.in_path+'/'
        endif
        
        ;--- for OUT_PATH parameter:
        if strpos(param, 'OUT_PATH') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.out_path = strtrim(strmid(s,9,last),2)
            ll = strlen(p.out_path)
            if (ll eq 0) then p.out_path = p.in_path $
            else if (strmid(p.out_path,ll-1) ne '/') then $
              p.out_path = p.out_path+'/'
        endif
        
        ;--- for VOE_PATH parameter:
        if strpos(param, 'VOE_PATH') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.voe_path = strtrim(strmid(s,9,last),2)
            ll = strlen(p.voe_path)
            if (ll eq 0) then p.voe_path = p.out_path $
            else if (strmid(p.voe_path,ll-1) ne '/') then $
              p.voe_path = p.voe_path+'/'
        endif
        
        ;--- for MASK_DEG parameter:
        if strpos(param, 'MASK_DEG') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.mask_deg = fix(strmid(s,9,last))
        endif

        ;--- for I_THRSH parameter:
        if strpos(param, 'I_THRSH') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.i_thrsh = fix(strmid(s,9,last))
            if not(u_thr_set) then p.u_thrsh = p.i_thrsh
        endif

        ;--- for U_THRSH parameter:
        if strpos(param, 'U_THRSH') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.u_thrsh = fix(strmid(s,9,last))
            u_thr_set = 1
        endif

        ;--- for MIN_AREA parameter:
        if strpos(param, 'MIN_AREA') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.min_area = fix(strmid(s,9,last))
        endif

        ;--- for DISP_DIM parameter:
        if strpos(param, 'DISP_DIM') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.disp_dim = float(strmid(s,9,last))
        endif

        ;--- for START_X parameter:
        if strpos(param, 'START_X') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.startx = fix(strmid(s,9,last))
        endif

        ;--- for START_Y parameter:
        if strpos(param, 'START_Y') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.starty = fix(strmid(s,9,last))
        endif

        ;--- for WIDTH parameter:
        if strpos(param, 'WIDTH') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.width = fix(strmid(s,9,last))
        endif

        ;--- for HEIGHT parameter:
        if strpos(param, 'HEIGHT') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.height = fix(strmid(s,9,last))
        endif

        ;--- for SMTH_IMG parameter:
        if strpos(param, 'SMTH_IMG') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.smth_img = fix(strmid(s,9,last))
        endif

        ;--- for BARB_ANG parameter:
        if strpos(param, 'BARB_ANG') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.barb_ang = float(strmid(s,9,last))
        endif

        ;--- for MAX_DIST parameter:
        if strpos(param, 'MAX_DIST') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.max_dist = fix(strmid(s,9,last))
        endif

        ;--- for FIX_BGR parameter:
        if strpos(param, 'FIX_BGR') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.fix_bgr = fix(strmid(s,9,last))
        endif

        ;--- for SHOW_PIX parameter:
        if strpos(param, 'SHOW_PIX') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.show_pix = fix(strmid(s,9,last))
        endif

        ;--- for SHOW_BBX parameter:
        if strpos(param, 'SHOW_BBX') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.show_bbx = fix(strmid(s,9,last))
        endif

        ;--- for SHOW_BND parameter:
        if strpos(param, 'SHOW_BND') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.show_bnd = fix(strmid(s,9,last))
        endif

        ;--- for SHOW_SPN parameter:
        if strpos(param, 'SHOW_SPN') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.show_spn = fix(strmid(s,9,last))
        endif

        ;--- for DO_VOE parameter:
        if strpos(param, 'DO_VOE') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.do_voe = fix(strmid(s,9,last))
        endif

        ;--- for VOE_UPDT parameter:
        if strpos(param, 'VOE_UPDT') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.voe_update = fix(strmid(s,9,last))
        endif

        ;--- for DO_TRACK parameter:
        if strpos(param, 'DO_TRACK') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.do_track = fix(strmid(s,9,last))
        endif

        ;--- for SAVE_TAB parameter:
        if strpos(param, 'SAVE_TAB') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.save_tab = fix(strmid(s,9,last))
        endif

        ;--- for SAVE_MSK parameter:
        if strpos(param, 'SAVE_MSK') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.save_msk = fix(strmid(s,9,last))
        endif

        ;--- for SAVE_PNG parameter:
        if strpos(param, 'SAVE_PNG') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.save_png = fix(strmid(s,9,last))
        endif

        ;--- for SAVE_FIL parameter:
        if strpos(param, 'SAVE_FIL') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.save_fil = fix(strmid(s,9,last))
        endif

        ;--- for SAVE_DAT parameter:
        if strpos(param, 'SAVE_DAT') ge 0 then begin
            last = strpos(s,';*')-9
            if (last lt 0) then last=strlen(s)
            p.save_dat = fix(strmid(s,9,last))
        endif

    endif
endwhile
close,unit
free_lun,unit
if (image_found ne 1) then message, 'IMAGE prameter missing in input file!'

if keyword_set(verbose) then begin
    print,'Input parameters:
    print,'    IMAGE    = "', p.image,'"'
    print,'    IN_PATH  = "', p.in_path,'"'
    print,'    OUT_PATH = "', p.out_path,'"'
    print,'    VOE_PATH = "', p.voe_path,'"'
    print,'    MASK_DEG =',p.mask_deg
    print,'    I_THRSH  =',p.i_thrsh
    print,'    U_THRSH  =',p.u_thrsh
    print,'    MIN_AREA =',p.min_area
    print,'    DISP_DIM =',p.disp_dim
    print,'    START_X  =',p.startx
    print,'    START_Y  =',p.starty
    if (p.width gt 0) then print,'    WIDTH    =',p.width
    if (p.height gt 0) then print,'    HEIGHT   =',p.height
    print,'    SMTH_IMG =',p.smth_img
    print,'    BARB_ANG =',p.barb_ang
    print,'    MAX_DIST =',p.max_dist
    print,'    FIX_BGR  =',p.fix_bgr
    print,'    SHOW_PIX =',p.show_pix
    print,'    SHOW_BBX =',p.show_bbx
    print,'    SHOW_BND =',p.show_bnd
    print,'    SHOW_SPN =',p.show_spn
    print,'    DO_VOE   =',p.do_voe
    print,'    VOE_UPDT =',p.voe_update
    print,'    DO_TRACK =',p.do_track
    print,'    SAVE_TAB =',p.save_tab
    print,'    SAVE_MSK =',p.save_msk
    print,'    SAVE_PNG =',p.save_png
    print,'    SAVE_FIL =',p.save_fil
    print,'    SAVE_DAT =',p.save_dat
    print
endif

return,p

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: read_input.pro,v $
; Revision 3.8  2010/05/06 14:33:49  bernapn1
; Several changes and improvements.  Now using a new technique to track
; filaments
;
; Revision 3.7  2010/04/29 20:59:14  bernapn1
; Added handling of new input keywords DO_TRACK and SAVE_MSK
;
; Revision 3.6  2010/03/26 19:08:08  bernapn1
; Added logic to hanle the new input parameter SAVE_FIL
;
; Revision 3.5  2010/03/08 21:21:55  bernapn1
; Improved
;
; Revision 3.4  2010/02/04 19:55:27  bernapn1
; Updated the screen output messages
;
; Revision 3.3  2010/01/21 21:52:23  bernapn1
; Added capability to read MASK_DEG, DO_VOE, and VOE_PATH from input file
;
; Revision 3.2  2009/12/18 21:52:24  bernapn1
; Improved fainter filaments detection
; Improved connection of nearby filaments
; Added MAX_DIST paramenter in input file (adds more flexibility)
;
; Revision 3.1  2008/11/18 18:49:01  bernapn1
; Fixed small bug: the initialization of image_found was wrong.
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.2  2004/08/24 20:13:56  pietro
; Added cvs log at bottom of file for modification history tracking
;
;    06/10/04 PNB: Removed C_PATH keyword. Now handling reading error with
;        ERROR keyword in openr.
;    05/05/04 PNB: Added input parameter SAVE_DAT
;    04/07/04 PNB: Changed THRSH into I_THRSH and added U_THRSH.
;    04/05/04 PNB: Added save_png variable in the parameters structure.
;    Written by Pietro N. Bernasconi JHU/APL: February 12, 2004
