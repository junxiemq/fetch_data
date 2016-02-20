subroutine readsac(name,sig,nlen,dt,beg,stla,stlo,nzhour,nzmin,nzsec,nzmsec,khole,nerr)
parameter (nn=21048576)
real sig(1),dt,beg,stla,stlo
integer nzhour,nzmin,nzsec,nzmsec,nnh
character(*)name
character(2)khole
integer nlen,nerr
!write(*,'("read file",1x,1a)')trim(name)
call rsac1(trim(name),sig,nlen,beg,dt,nn,nerr)
if (nerr.ne.0) then
      write(*,*)'nerr=',nerr
      write(*,'("Error in reading sac file",1x,1a)')trim(name)
      return
endif
call getfhv('stla',  stla,  nerr)
call getfhv('stlo',  stlo,  nerr)
call getnhv('nzhour',nzhour,nerr)
call getnhv('nzmin', nzmin, nerr)
call getnhv('nzsec', nzsec, nerr)
call getnhv('nzmsec',nzmsec,nerr)
call getkhv('khole', khole, nerr)
return
end subroutine
