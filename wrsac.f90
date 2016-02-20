subroutine  wrsac(name,dt,y,nsmpl,stla,stlo,nzyear,nzjday,nzhour,nzmin,nzsec,nzmsec,sta,net,com,khole,nerr)
real y(1),b,dt,gcarc,evdp,baz,sum
integer nsmpl,nzjday,nzyear
integer nerr,i
character*(*) name,sta,net,com
character(8) khole
call newhdr
call setnhv('npts',  nsmpl, nerr)
call setnhv('nzhour',nzhour,nerr)
call setnhv('nzmin', nzmin, nerr)
call setnhv('nzsec', nzsec, nerr)
call setnhv('nzmsec',nzmsec,nerr)
call setnhv('nzyear',nzyear,nerr)
call setnhv('nzjday',nzjday,nerr)
call setfhv('delta', dt,    nerr)
call setfhv('stla', stla,    nerr)
call setfhv('stlo', stlo,    nerr)
call setkhv('KNETWK',net,nerr)
call setkhv('KCMPNM',com,nerr)
call setkhv('KSTNM',sta,nerr)
call setkhv('KHOLE',trim(khole),nerr)
!write(*,*)'in writeout',nzhour,nzmin,nzsec,nzmsec
b=0
e=b+(nsmpl-1)*dt
o=0.
do i=1,nsmpl
      sum=sum+y(i)/nsmpl
enddo
y=y-sum
call setfhv('e',e,nerr)
call setfhv('o',o,nerr)
call setfhv('b',b,nerr)
!      !     call setihv('iztype','io',nerr)
call wsac0(name,dum,y,nerr)
if(nerr.ne.0) stop 'Error in writing output file'
return
end subroutine wrsac
