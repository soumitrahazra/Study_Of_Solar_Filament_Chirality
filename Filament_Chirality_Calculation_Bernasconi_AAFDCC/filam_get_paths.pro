;*** This routine is automatically created.
;*** It is useless and unnecessary to mofity it!
function filam_get_paths,HOME=home,BIN=bin,RESULTS=results, $
IMAGES=images, IDL=idldir, VOE=voe
case 1 of
    keyword_set(home): return,'/home/bernapn/filaments/'
    keyword_set(bin): return,'/home/bernapn/filaments/src/bin/'
    keyword_set(results): return,'/home/bernapn/filaments/results/'
    keyword_set(images): return,'/home/bernapn/filaments/images/'
    keyword_set(idldir): return,'/home/bernapn/filaments/src/idl/'
    keyword_set(voe): return,'/home/bernapn/filaments/VOEvents/'
    else: print,'%FILAM_GET_PATHS: Nothig selected! Returning empty string!'
endcase
return,''
end
