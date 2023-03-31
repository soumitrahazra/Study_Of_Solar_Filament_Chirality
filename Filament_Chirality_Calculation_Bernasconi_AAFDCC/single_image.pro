pro single_image, CurrentFile
;  CurrentFile = 'resized_bbso_halph_fr_19990502_153322.jpg.fits'
;  CurrentDir = '/media/data2/filaments/20111530'
  CurrentDir = '/home/bernapn/filaments/src/idl'
  status = analyze_image(CurrentFile, $
			 IN_PATH = CurrentDir, $
			 RESULTS_PATH = CurrentDir, $
			 /SAVE_TABLE, /SAVE_MASK, $
			 /VERBOSE,/SAVE_PNG,/SAVE_DAT)
  help, status
end
			 
