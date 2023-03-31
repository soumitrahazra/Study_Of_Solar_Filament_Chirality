;******************************************************************************
pro interactive_disp_help
  print,'    List of commands (case insensitive):'
  print,'        q = QUIT interactive mode'
  print,'        h = HELP: prints these commands again'
  print,'        ? = STATUS: print current variables status'
  print,'        m = Asks for a new magnification factor'
  print,'        z = ZOOM: asks to click on the lower left and the ', $
    '            upper right corner of the area to be viewed. It is then ', $
    '            displayed with the current tly se magnification factor.'
  print,'        s = SAVE: save window in a png file'
  print,'        c = CLEAR: clears the original window from clutter'
end
;******************************************************************************

;******************************************************************************
;**** Internal routine used to dispay the image
pro draw_image, fil_data, disp_img, magn, window, XOFFSET=xoff, YOFFSET=yoff,$
                SUN_PARAMS = sun_p, SHOW_ALL_BRBS=show_all_brbs
  if (n_elements(xoff) le 0) then xoff = 0
  if (n_elements(yoff) le 0) then yoff = 0
  if (n_elements(sun_p) lt 7) then sun_p = [0,0,0,0,0,0,0]

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
  ;--- Sets the right color code
  if (true_color) then begin
      ;--- This are the setting for a monitor with True Color 
      red_c = fsc_color('red')
      green_c = fsc_color('green')
      yellow_c = fsc_color('yellow')
      cyan_c = fsc_color('cyan')
  endif else begin
      ;--- This are the setting for a standard monitor without True Color 
      red_c = ctb_color('red',c_tab)
      green_c = ctb_color('green', c_tab)
      yellow_c = ctb_color('yellow', c_tab)
      cyan_c = ctb_color('cyan', c_tab)
  endelse

  szf = size(fil_data)
  szd = size(disp_img)
  wset, window
  wshow, window
  tv,bytscl(congrid(disp_img, szd(1)*magn, szd(2)*magn, /int), $
            min=-1500, max=1500, top=grey_lim)
  if (sun_p(0) gt 0) then begin      
      plot_helio_grid, sun_p(0)*magn, $
        sun_c=[sun_p(1)-xoff,sun_p(2)-yoff]*magn, B0=sun_p(6), $
        /noer, color=green_c, line=2

  endif
  for i=0, szf(1)-1 do begin;
      ;--- Draw filament boundary in yellow
      plots, (fil_data(i).bnd(0,0:fil_data(i).nbnd_p-1) - xoff) * magn, $
        (fil_data(i).bnd(1,0:fil_data(i).nbnd_p-1) - yoff ) * magn , $
        /Device, Color=yellow_c

      ;--- Draw filament spine in cyan
      plots, (fil_data(i).spine(0,0:fil_data(i).nverts-1) - xoff) * magn, $
        (fil_data(i).spine(1,0:fil_data(i).nverts-1) - yoff) * magn, $
        /Device, Color=cyan_c

      ;--- draw the single barbs:
      if (fil_data(i).nbarbs gt 0) then begin
          for b=0, fil_data(i).nbarbs-1 do begin
              plots, (fil_data(i).brb_coord(0,*,b) - xoff) * magn, $
                (fil_data(i).brb_coord(1,*,b) - yoff) * magn, $
                /Device, color=green_c

              case 1 of
                  fil_data(i).barbdir(b) eq -1: d_str = ' R'
                  fil_data(i).barbdir(b) eq  1: d_str = ' L'
                  else : d_str = ''
              endcase
              xyouts,(fil_data(i).brb_coord(0,0,b) - xoff) * magn, $
                (fil_data(i).brb_coord(1,0,b) - yoff) * magn, $
                d_str,/dev, color=cyan_c
          endfor
      endif

      ;--- marks the filament coordinates location with an asterisk
      plots,[sun_p(1) + fil_data(i).coords(0) - xoff, $
             sun_p(2) + fil_data(i).coords(1) - yoff] * magn, psym=2, $
        /Device, Color=red_c

      ;--- Prints the filament number and the chirality statistics:
      case 1 of
          (fil_data(i).chir eq -1): chirstr = 'L'
          (fil_data(i).chir eq  1): chirstr = 'R'
          else: chirstr = '?'
      endcase
      fs='(I2)'
      xyostr = string(i+1,format='(I3,": ")') + $
        chirstr + ' ('+string(fil_data(i).nRight,format=fs) +'R,' + $
        string(fil_data(i).nLeft,format=fs) + 'L)'
      mincrds=[ min(fil_data(i).bnd(0,0:fil_data(i).nbnd_p-1)) - xoff , $
                min(fil_data(i).bnd(1,0:fil_data(i).nbnd_p-1)) - yoff ]
      xyouts, mincrds(0) * magn + 5, mincrds(1) * magn - 12, xyostr, $
        /device, color=1, charthick=2, charsize=1.3
  endfor
end
;******************************************************************************


;******************************************************************************
;******************************************************************************
;***                                                                        ***
      pro interactive_disp, fil_data, disp_img, FILE_INPUT=infile, $
                            MAGNIFICATION = magn, SUN_PARAMS = sun_p, $
                            CORRECT_IMAGE = corr_img, $
                            SHOW_ALL_BRBS=show_all_brbs
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    INTERACTIVE_DISP
;
; PURPOSE:
;    This is a routine that performs some interactive display operations
;
; CALLING SEQUENCE:
;    interactive_disp, filam_data, disp_data [, FILE_INPUT=infile, $
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
;                     to remove residual inhomogeneitier (SLOW!)
;
; OUTPUTS:
;    None
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    read_filam_data, plot_helio_grid
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_elements(infile) gt 0) then $
  ifil_data = read_filam_data(infile, sun_p = sun_p, disp_image = idisp_img, $
                              CORRECT_IMAGE=corr_img)

;--- If infile not present or something is wrong then try the input parameters:
if (size(ifil_data,/type) ne 8) then begin
    if (n_params() lt 2) then message,'required input parameter missing'

    if (size(fil_data,/type) ne 8 ) then $
      message,'1st parameter must be a structure!'

    szd = size(disp_img)
    if (szd(0) ne 2) then $
      message,'dispay image (2nd parameter) must be a 2D array!'
endif else begin
    fil_data = ifil_data
    disp_img = idisp_img
    szd = size(disp_img)
endelse

if (n_elements(magn) le 0) then magn = 1.
magn=float(magn)

print
print,'ENTERING INTERACTIVE DISPLAY'
print,'    Current magnification factor =', magn
interactive_disp_help

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

orig_magn = magn

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
    draw_image, fil_data, disp_img, orig_magn, orig_w, sun_p=sun_p,$
      SHOW_ALL_BRBS=show_all_brbs,/as_is
endif

png_fnam=''
repeat begin
    print
    cmd = strlowcase(get_kbrd(1))
    case 1 of
        (cmd eq 'h'): interactive_disp_help

        (cmd eq 'm'): begin
            print,format='("    Enter new magnification factor",$)'
            read,magn
            print,'    New magnification factor =', magn
        end

        (cmd eq '?'): begin
            print,'    Current magnification factor =', magn
        end

        (cmd eq 'z'): begin
            wset, orig_w
            wshow, orig_w
 
            ;--- Gets corners of box from cursor:
            print,'    Magnify a box within the original window:'
            print,'        Click on window to select one corner ...'
            cursor, xll, yll, /dev, /up
            print,'        Click on window to select opposite corner ...'
            cursor, xur, yur, /dev, /up

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
            
            xoff = fix(xll/orig_magn + 0.5)
            yoff = fix(yll/orig_magn + 0.5)
            xsz  = fix((xur-xll)/orig_magn + 0.5)
            if (xoff+xsz gt szd(1)-1) then xsz = szd(1)-xoff-1
            ysz  = fix((yur-yll)/orig_magn + 0.5)
            if (yoff+ysz gt szd(2)-1) then ysz = szd(2)-yoff-1

            magn_w = 31
            window,magn_w,xs=xsz*magn,ys=ysz*magn, title = 'Detail'
            draw_image, fil_data, disp_img(xoff:xoff+xsz,yoff:yoff+ysz),$
              magn, magn_w, xoff=xoff, yoff=yoff, sun_p=sun_p, $
              SHOW_ALL_BRBS=show_all_brbs
        end

        (cmd eq 'c'): begin
            print,'    Redraw original window ...'
            draw_image, fil_data, disp_img, orig_magn, orig_w, sun_p=sun_p, $
              SHOW_ALL_BRBS=show_all_brbs
        end

        (cmd eq 's'): begin
            print,'    Save window in png file:'
            if (magn_w ne -1) then begin
                print,'        save Main window (m) or Detail window (d)?'
                if (strlowcase(get_kbrd(1)) eq 'd') then wset,magn_w $
                  else wset, orig_w
            endif
            print,format='("        enter name of file",$)'
            read,png_fnam
            if true_color then begin
                tmp_im = tvrd(true=1,/order)
                write_png,png_fnam,tmp_im
            endif else begin
                tmp_im = tvrd(/order)
                write_png,png_fnam,tmp_im,c_red,c_green,c_blue
            endelse
            print,'        image written in file ',png_fnam
        end

        (cmd eq 'q'): print,'QUITTING INTERACTIVE DISPLAY'

        else:
    endcase
endrep until (cmd eq 'q')

if (magn_w ne -1) then wdelete, magn_w

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: interactive_disp.pro,v $
; Revision 3.1  2010/10/27 19:47:09  bernapn1
; Added call to keyword as_is in call to routine draw_image
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.2  2004/06/22 14:03:49  pietro
; Fixed bug regarding the creation of a png image when display is not a
; 	true color display
;
; Revision 2.1  2004/06/21 21:29:00  pietro
; Now can handle also NON True Color displays
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.4  2004/06/16 20:33:09  pietro
; don't know
;
; Revision 1.3  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL:
