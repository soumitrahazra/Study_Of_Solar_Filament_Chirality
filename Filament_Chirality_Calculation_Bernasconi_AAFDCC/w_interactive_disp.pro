;******************************************************************************
      pro w_interactive_disp_event, ev

common f_data, fil_data, disp_img, magn, orig_magn, orig_w, magn_w, sun_p
common colortab, c_tab, grey_lim, c_red, c_green, c_blue
if (n_elements(c_tab) le 0) then begin
    if (!d.n_colors le 256) then begin
        true_color = 0
        c_tab = make_colortab(grey_limit=grey_lim, red=c_red, $
                              green=c_green, blue=c_blue)
    endif else true_color = 1
endif else true_color = 0
;--- Sets the right color code
if (true_color) then begin 
    ;--- Setting for a monitor with True Color 
    red_c = fsc_color('red')
endif else begin
    ;--- Setting for a standard monitor without True Color 
    red_c = ctb_color('red',c_tab)
endelse

widget_control, ev.top, get_uvalue=state

case ev.id of
    ;--- Some text has been entered in text box:
    state.w_magn_valID: begin
        widget_control, ev.id, get_value = val_mag
        if (strlen(val_mag(0)) gt 0) then $
          widget_control, state.w_magn_enterID, /sensitive $
        else $
          widget_control, state.w_magn_enterID, sensitive = 0
    end

    ;--- The SET button has been hit:
    state.w_magn_enterID: begin
        widget_control, state.w_magn_valID, get_value = val_mag
        if (strlen(val_mag(0)) gt 0) then begin
            widget_control, state.w_magn_valID, set_value=''
            widget_control, state.w_magn_enterID, sensitive = 0

            valid = 0
            on_ioerror, bad_number
            new_magn = float(val_mag)
            valid = 1
            bad_number: if not valid then $
              print,'BAD NUMBER!' $
            else begin
                magn = new_magn(0)
                s = string(format='(" Magnification factor: ", f5.2)', magn)
                widget_control, state.w_magn_labID, set_value=s
            endelse
        endif
    end

    ;--- The ZOOM button has been hit:
    state.w_zoomID: begin

        wset, orig_w  &  wshow, orig_w

        ;--- Gets corners of box from cursor:
        widget_control, state.w_zoom_commID, $
          set_value='Select one corner'
        cursor, xll, yll, /dev, /up
        widget_control, state.w_zoom_commID, $
           set_value='Select opposite corner'
        cursor, xur, yur, /dev, /up
        widget_control, state.w_zoom_commID, set_value=' '

        ;--- Swap x coords if necessary
        if (xur lt xll) then begin
            tmp = xll
            xll = xur
            xur = tmp
        endif
        ;--- Swap y coords if necessary
        if (yur lt yll) then begin
            tmp = yll
            yll = yur
            yur = tmp
        endif
        plots,[xll, xur, xur, xll, xll], [yll, yll, yur, yur, yll], $
          /dev, color=red_c

        szd=size(disp_img)
        xoff = fix(xll/orig_magn + 0.5)
        yoff = fix(yll/orig_magn + 0.5)
        xsz  = fix((xur-xll)/orig_magn + 0.5)
        if (xoff+xsz gt szd(1)-1) then xsz = szd(1)-xoff-1
        ysz  = fix((yur-yll)/orig_magn + 0.5)
        if (yoff+ysz gt szd(2)-1) then ysz = szd(2)-yoff-1

        magn_w = 20
        window,magn_w,xs=xsz*magn,ys=ysz*magn, title = 'Detail'
        draw_image, fil_data, disp_img, sun_p, magn_w, magn, $
          xoff=xoff, yoff=yoff, xsz=xsz, ysz=ysz, $
          /SHOW_BOUND, /SHOW_SPINE, SHOW_ALL_BRBS=show_all_brbs
    end

    ;--- The CLEAR button has been hit:
    state.w_clearID: begin
        draw_image, fil_data, disp_img, sun_p, orig_w, orig_magn, $
          /SHOW_BOUND, /SHOW_SPINE, SHOW_ALL_BRBS=show_all_brbs
    end

    ;--- The SAVE button has been hit:
    state.w_saveID : begin
        if (magn_w ne -1) then begin
            answer = dialog_message('Save the Detail window?', /question,$
                                    dialog_parent=ev.top)
            if (answer eq 'Yes') then $
              wset,magn_w $
            else $
              wset, orig_w
        endif
        png_fnam = dialog_pickfile(/write, dialog_parent=ev.top)
        if (strlen(png_fnam) gt 0) then begin
            if true_color then begin
                tmp_im = tvrd(true=1,/order)
                write_png,png_fnam,tmp_im
            endif else begin
                tmp_im = tvrd(/order)
                write_png,png_fnam,tmp_im,c_red,c_green,c_blue
            endelse
        endif
    end

    ;--- The 'QUIT' button was pressed:
    state.w_quitID: begin
        answer = dialog_message('Do you want to DELETE the main window?', $
                                /question, /default_no, dialog_parent=ev.top)
        widget_control, ev.top, /destroy
        if (magn_w ne -1) then wdelete, magn_w
        if (answer eq 'Yes') then  wdelete, orig_w
    end
endcase

end
;******************************************************************************

;******************************************************************************
;******************************************************************************
;***                                                                        ***
      pro w_interactive_disp, ifil_data, idisp_img, FILE_INPUT=infile, $
                            MAGNIFICATION = imagn, SUN_PARAMS = isun_p, $
                            CORRECT_IMAGE = corr_img, $
                            SHOW_ALL_BRBS=show_all_brbs
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    W_INTERACTIVE_DISP
;
; PURPOSE:
;    This is a routine that performs some interactive display operations.
;    A widget window gets opened and displays a menu with all the possible
;    options accepted by this routine.
;
; CALLING SEQUENCE:
;    w_interactive_disp, filam_data, disp_data [, FILE_INPUT=infile, $
;                      MAGNIFICATION = factor, SUN_PARAMS = sun_p, $
;                      /CORRECT_IMAGE]
;
; INPUTS:
;    filam_data = structure_array(nFilaments): structure containing all the
;                    filaments data. See study_filaments for a detailed
;                    description of the structure content.
;    disp_data  = array(X,Y): image for display
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    FILE_INPUT = string: Name of the .dat file with all the parameters. If
;                     this is given then filam_data and disp_data are
;                     ignored. The file can also be compressed in gzip
;                     format, in which case the suffix must be .gz !
;    MAGNIFICATION = float: magnification factor. Default = 1
;    SUN_PARAMS = intarr(3): solar parameters for display:
;                     0) Sun radius; 1) X coord of center; 2) Y coord of center
;    CORRECT_IMAGE = flag: if set then performs a fit of the image surface
;                     to remove residual inhomogeneities (SLOW!)
;
; OUTPUTS:
;    None
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    read_filam_data, plot_helio_grid, draw_image
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
common f_data, fil_data, disp_img, magn, orig_magn, orig_w, magn_w, sun_p

;--- Gets the input data from a file:
if (n_elements(infile) gt 0) then begin
    nfil_data = read_filam_data(infile, sun_p = nsun_p, $
                                disp_image = ndisp_img, CORRECT_IMAGE=corr_img)

    ;--- If infile not present or something is wrong then try the input
    ;    parameters:
    if (size(nfil_data,/type) ne 8) then begin
        if (n_params() lt 2) then message,'required input parameter missing'

        if (size(ifil_data,/type) ne 8 ) then $
          message,'1st parameter must be a structure!'

        szd = size(idisp_img)
        if (szd(0) ne 2) then $
          message,'dispay image (2nd parameter) must be a 2D array!'
    endif else begin
        fil_data = nfil_data
        disp_img = ndisp_img
        szd = size(disp_img)
        if (n_elements(nsun_p) lt 7) then $
          sun_p = [0,0,0,0,0,0,0] $
        else $
          sun_p = nsun_p
    endelse
endif else begin
    fil_data = ifil_data
    disp_img = idisp_img
    szd = size(disp_img)
    if (n_elements(isun_p) lt 7) then $
      sun_p = [0,0,0,0,0,0,0] $
    else $
      sun_p = isun_p
endelse

if (n_elements(imagn) le 0) then $
  magn = 1. $
else $
  magn=float(imagn)
orig_magn = magn

;--- Checks is the common block is defined. If not then checks what color
;    resolution it is. If present then display is NOT a true color
common colortab, c_tab, grey_lim, c_red, c_green, c_blue
if (n_elements(c_tab) le 0) then begin
    if (!d.n_colors le 256) then begin
        true_color = 0
        c_tab = make_colortab(grey_limit=grey_lim, red=c_red, $
                              green=c_green, blue=c_blue)
    endif else true_color = 1
endif else true_color = 0
if (true_color) then begin
    red_c = fsc_color('red')
endif else begin
    red_c = ctb_color('red',c_tab)
endelse

;-- Setup for the windows used:
neworig = 0
orig_w = !d.window
magn_w = -1
if (orig_w lt 0) then begin
    orig_w = 0
    neworig = 1
endif
if ((!d.x_size ne szd(1)*orig_magn) or (!d.y_size ne szd(2)*orig_magn)) then $
  neworig = 1

if neworig then begin
    window, orig_w, xs=szd(1)*orig_magn, ys=szd(2)*orig_magn, $
      title = 'Original window'
    draw_image, fil_data, disp_img, sun_p, orig_w, orig_magn, $
      /SHOW_BOUND, /SHOW_SPINE, SHOW_ALL_BRBS=show_all_brbs
endif

;--- Entering Interactive Display Window setup --------------------------------
base=widget_base(/column, title='Interactive Dispaly Menu')

s = string(format='(" Magnification factor: ", f5.2)', magn)
w_magn_labID = widget_label(base, value=s)

;-- Creates sublayer: a row for entering the magnification factor
w_magn_base = widget_base(base, /row, frame=1)
w_label = widget_label(w_magn_base, value='New magn factor:')
w_magn_valID = widget_text(w_magn_base, /editable, /ALL_EVENTS, xsize=8)
w_magn_enterID = widget_button(w_magn_base, value=' SET ')
widget_control, w_magn_enterID, sensitive = 0   ;<- Disabeling the enter button

;-- Creates sublayer: a row for the zoom
w_zoom_base = widget_base(base, /row)
w_zoomID = widget_button(w_zoom_base, value=' ZOOM ')
w_zoom_commID = widget_label(w_zoom_base, value='                         ')

;-- The last buttons:
w_saveID=widget_button(base, value = ' SAVE IMAGE IN PNG FILE ')
w_clearID=widget_button(base, value = ' CLEAR MAIN IMAGE ')
w_quitID=widget_button(base, value = ' QUIT Interactive Display ')

state = {w_magn_labID:w_magn_labID, $
         w_magn_valID:w_magn_valID, $
         w_magn_enterID:w_magn_enterID, $
         w_zoomID:w_zoomID, $
         w_zoom_commID:w_zoom_commID, $
         w_saveID:w_saveID, $
         w_clearID:w_clearID, $
         w_quitID:w_quitID}

widget_control, base, /realize
widget_control, base, set_uvalue = state
xmanager, 'w_interactive_disp', base
end
;******************************************************************************

;  MODIFICATION HISTORY:
; $Log: w_interactive_disp.pro,v $
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.4  2004/07/13 15:34:05  pietro
; Now draw_image is no longer an internal routine but it is a standalone one.
;
; Revision 2.3  2004/07/12 15:15:15  pietro
; Now plots dots to show the boundary
;
; Revision 2.2  2004/06/25 18:07:04  pietro
; Now it handels properly the option when button cancel is hit when the save
; 	file window  is active. On QUIT also the main window is closed.
;
; Revision 2.1  2004/06/25 17:37:56  pietro
; Created starting from the routine 'interactive_disp'
;
